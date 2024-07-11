//
// Generic plotter for BESM-6.
//
// Copyright (c) 2024 Serge Vakulenko
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
#include <cerrno>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include "machine.h"

//
// Finish operation.
// Save all data files.
//
void Plotter::finish()
{
    if (!watanabe.empty()) {
        save_to_file("watanabe", watanabe);
        watanabe_convert_svg("watanabe");
        watanabe.erase();
    }
    if (!calcomp.empty()) {
        save_to_file("calcomp", calcomp);
        calcomp_convert_svg("calcomp");
        calcomp.erase();
    }
    if (!tektronix.empty()) {
        save_to_file("tektronix", tektronix);
        tektronix_convert_svg("tektronix");
        tektronix.erase();
    }
}

//
// Finish current page and start new one.
//
void Plotter::change_page()
{
    if (watanabe.empty() && calcomp.empty() && tektronix.empty()) {
        // No reason to increase page number.
        return;
    }
    if (page_number == 0) {
        // Enable page numbers.
        page_number = 1;
    }
    finish();
    page_number += 1;
}

//
// Append suffix to a file base name.
// Also optionally append a page number.
//
std::string Plotter::append_suffix(const std::string &basename, const std::string &suffix)
{
    if (page_number > 0) {
        // Page numbering is enabled.
        return basename + std::to_string(page_number) + suffix;
    }
    return basename + suffix;
}

//
// Save raw output.
//
void Plotter::save_to_file(std::string filename, const std::string &data)
{
    filename = append_suffix(filename, ".out");
    std::ofstream out(filename);
    if (!out.is_open()) {
        std::cerr << filename << ": " << std::strerror(errno) << std::endl;
        return;
    }
    out << data;
}

//
// Convert Watanabe output to SVG format.
//
void Plotter::watanabe_convert_svg(std::string filename)
{
    // Default color is black.
    unsigned color = 1;
    static const std::string color_name[6] = {
        "black",     // pen #1 - black
        "red",       // pen #2 - red
        "green",     // pen #3 - green
        "blue",      // pen #4 - blue
        "goldenrod", // pen #5 - yellow
        "darkcyan",  // pen #6 - cyan
    };

    // Get dimensions.
    unsigned maxx{}, maxy{};
    watanabe_parse([&](char ch, unsigned x, unsigned y) {
        if (ch == 'J')
            return;
        if (x > maxx)
            maxx = x;
        if (y > maxy)
            maxy = y;
    });

    // Open file for SVG output.
    filename = append_suffix(filename, ".svg");
    std::ofstream out(filename);
    if (!out.is_open()) {
        std::cerr << filename << ": " << std::strerror(errno) << std::endl;
        return;
    }

    // Write SVG header.
    out << "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
        << "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:ev=\"http://www.w3.org/2001/xml-events\""
        << " xmlns:xlink=\"http://www.w3.org/1999/xlink\" baseProfile=\"full\" version=\"1.1\""
        << " width=\"" << (maxx+1)
        << "\" height=\"" << (maxy+1)
        << "\" viewBox=\"0 0 " << (maxx+1)
        << " " << (maxy+1) << "\">\n"
        << "<g fill=\"none\" stroke=\"black\" stroke-linecap=\"round\" stroke-width=\"3\">\n";

    // Write lines.
    std::vector<std::pair<unsigned, unsigned>> path;
    watanabe_parse([&](char ch, unsigned x, unsigned y) {
        if (ch != 'D') {
            // Write path.
            if (path.size() > 1) {
                out << "<path d=\"M";
                for (auto const &item : path) {
                    out << ' ' << item.first << ' ' << item.second;
                }
                out << "\"/>\n";
            }
            if (ch == 'J' && path.size() > 0) {
                // Restore last position.
                auto last = path.back();
                path.clear();
                path.push_back(last);
            } else {
                path.clear();
            }
        }
        if (ch == 'J') {
            // Change color.
            if (x >= 1 && x <= 6 && x != color) {
                color = x;
                out << "</g>\n"
                    << "<g fill=\"none\" stroke=\"" << color_name[color-1]
                    << "\" stroke-linecap=\"round\" stroke-width=\"3\">\n";
            }
            return;
        }
        path.push_back({x, maxy - y});
    });

    // Write SVG footer.
    out << "</g>\n";
    out << "</svg>\n";
}

//
// Parse Watanabe file and invoke given routine for each line.
//
void Plotter::watanabe_parse(const std::function<void(char, unsigned, unsigned&)> &func)
{
    std::istringstream input(watanabe);
    std::string line;
    while (std::getline(input, line)) {
        if (line.size() < 1) {
            // Ignore empty line.
            continue;
        }
        char ch = line[0];
        line.erase(0, 1);
        if (ch == 'J') {
            // Select color.
            unsigned color = std::stoul(line);
            func(ch, color, color);
            continue;
        }
        unsigned x, y;
        int num_read = std::sscanf(line.c_str(), "%u,%u", &x, &y);
        if (num_read != 2) {
            // Ignore bad line.
            continue;
        }
        func(ch, x, y);
    }
}

//
// Convert Tektronix output to SVG format.
//
void Plotter::tektronix_convert_svg(std::string filename)
{
    // Get dimensions.
    unsigned maxx{}, maxy{};
    tektronix_parse([&](bool flag, unsigned x, unsigned y) {
        if (x > maxx)
            maxx = x;
        if (y > maxy)
            maxy = y;
    });

    // Open file for SVG output.
    filename = append_suffix(filename, ".svg");
    std::ofstream out(filename);
    if (!out.is_open()) {
        std::cerr << filename << ": " << std::strerror(errno) << std::endl;
        return;
    }

    // Write SVG header.
    out << "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
        << "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:ev=\"http://www.w3.org/2001/xml-events\""
        << " xmlns:xlink=\"http://www.w3.org/1999/xlink\" baseProfile=\"full\" version=\"1.1\""
        << " width=\"" << (maxx+1)
        << "\" height=\"" << (maxy+1)
        << "\" viewBox=\"0 0 " << (maxx+1)
        << " " << (maxy+1) << "\">\n"
        << "<g fill=\"none\" stroke=\"black\" stroke-linecap=\"round\" stroke-width=\"1.5\">\n";

    // Write lines.
    std::vector<std::pair<unsigned, unsigned>> path;
    tektronix_parse([&](bool new_path, unsigned x, unsigned y) {
        if (new_path) {
            if (path.size() > 1) {
                // Write path.
                out << "<path d=\"M";
                for (auto const &item : path) {
                    out << ' ' << item.first << ' ' << item.second;
                }
                out << "\"/>\n";
            }
            path.clear();
        }
        path.push_back({x, maxy - y});
    });

    // Write SVG footer.
    out << "</g>\n";
    out << "</svg>\n";
}

//
// Parse Tektronix file and invoke given routine for each line.
//
void Plotter::tektronix_parse(const std::function<void(bool, unsigned, unsigned&)> &func)
{
    static const char GS = 035;
    for (auto ptr = tektronix.cbegin(); ptr < tektronix.cend(); ) {
        bool flag = (*ptr == GS);
        if (flag) {
            ptr++;
        }
        unsigned h = (*ptr++ & 037) << 5;
        unsigned y = (*ptr++ & 037) | h;
        h = (*ptr++ & 037) << 5;
        unsigned x = (*ptr++ & 037) | h;
        func(flag, x, y);
    }
}

//
// Do two value have the same sign, or both zero?
//
static inline bool same_sign(int a, int b)
{
    if (a == 0)
        return (b == 0);
    return (a < 0) == (b < 0);
}

//
// Return true when vector (ax,ay) is collinear with vector (bx,by)
//
static inline bool is_collinear(int ax, int ay, int bx, int by)
{
    if (ax == 0) {
        return (bx == 0) && same_sign(ay, by);
    }
    if (ay == 0) {
        return (by == 0) && same_sign(ax, bx);
    }
    return (ax*by == ay*bx);
}

//
// Convert Calcomp output to SVG format.
//
void Plotter::calcomp_convert_svg(std::string filename)
{
    // Get dimensions.
    int x{}, y{}, minx{}, miny{}, maxx{}, maxy{};
    calcomp_parse([&](bool flag, int dx, int dy) {
        x += dx;
        y += dy;
        if (x < minx)
            minx = x;
        if (y < miny)
            miny = y;
        if (x > maxx)
            maxx = x;
        if (y > maxy)
            maxy = y;
    });

    // Open file for SVG output.
    filename = append_suffix(filename, ".svg");
    std::ofstream out(filename);
    if (!out.is_open()) {
        std::cerr << filename << ": " << std::strerror(errno) << std::endl;
        return;
    }

    // Write SVG header.
    int width = maxx + 1 - minx;
    int height = maxy + 1 - miny;
    out << "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
        << "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:ev=\"http://www.w3.org/2001/xml-events\""
        << " xmlns:xlink=\"http://www.w3.org/1999/xlink\" baseProfile=\"full\" version=\"1.1\""
        << " width=\"" << width
        << "\" height=\"" << height
        << "\" viewBox=\"0 0 " << width
        << " " << height << "\">\n"
        << "<g fill=\"none\" stroke=\"black\" stroke-linecap=\"round\" stroke-width=\"1\">\n";

    // Write lines.
    std::vector<std::pair<int, int>> path;
    x = 0;
    y = 0;
    calcomp_parse([&](bool new_path, int dx, int dy) {
        x += dx;
        y += dy;
        if (new_path) {
            if (path.size() >= 2) {
                // Write path.
                out << "<path d=\"M";
                for (auto const &item : path) {
                    out << ' ' << item.first << ' ' << item.second;
                }
                out << "\"/>\n";
            }
            path.clear();
        }
        auto dev_x = x - minx;
        auto dev_y = maxy - y;
        if (path.size() >= 2) {
            // Optimize the path.
            auto last = *std::prev(path.end());
            auto prev = *std::prev(std::prev(path.end()));
            auto ax   = dev_x - last.first;
            auto ay   = dev_y - last.second;
            auto bx   = last.first - prev.first;
            auto by   = last.second - prev.second;
            if (is_collinear(ax, ay, bx, by)) {
                // Ignore previous point it's exactly in the same direction.
                path.pop_back();
            }
        }
        path.push_back({dev_x, dev_y});
    });

    // Write SVG footer.
    out << "</g>\n";
    out << "</svg>\n";
}

//
// Parse Calcomp file and invoke given routine for each line.
//
void Plotter::calcomp_parse(const std::function<void(bool, int, int&)> &func)
{
    bool pen_up{};
    for (auto const byte : calcomp) {
        int dx{}, dy{};
        if (byte & 1)
            dx = -1;
        if (byte & 2)
            dx = 1;
        if (byte & 4)
            pen_up = true;
        if (byte & 010)
            pen_up = false;
        if (byte & 020)
            dy = 1;
        if (byte & 040)
            dy = -1;
        if (dx || dy)
            func(pen_up, dx, dy);
    }
}
