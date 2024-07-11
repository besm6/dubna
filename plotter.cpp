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
        save_to_file("watanabe.out", watanabe);
        watanabe_convert_svg("plotter.svg");
        watanabe.erase();
    }
    if (!calcomp.empty()) {
        save_to_file("calcomp.out", calcomp);
        calcomp_convert_svg("plotter.svg");
        calcomp.erase();
    }
    if (!tektronix.empty()) {
        save_to_file("tektronix.out", tektronix);
        tektronix_convert_svg("plotter.svg");
        tektronix.erase();
    }
}

//
// Save raw output.
//
void Plotter::save_to_file(const std::string &filename, const std::string &data)
{
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
void Plotter::watanabe_convert_svg(const std::string &filename)
{
    // Get dimensions.
    unsigned maxx{}, maxy{};
    watanabe_parse([&](char ch, unsigned x, unsigned y) {
        if (x > maxx)
            maxx = x;
        if (y > maxy)
            maxy = y;
    });

    // Open file for SVG output.
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
        if (ch == 'M') {
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
// Parse Watanabe file and invoke given routine for each line.
//
void Plotter::watanabe_parse(const std::function<void(char, unsigned, unsigned&)> &func)
{
    std::istringstream input(watanabe);
    std::string line;
    while (std::getline(input, line)) {
        char ch;
        unsigned x, y;
        int num_read = std::sscanf(line.c_str(), "%c%u,%u", &ch, &x, &y);
        if (num_read != 3) {
            // Ignore bad line.
            continue;
        }
        func(ch, x, y);
    }
}

//
// Convert Tektronix output to SVG format.
//
void Plotter::tektronix_convert_svg(const std::string &filename)
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
// Convert Calcomp output to SVG format.
//
void Plotter::calcomp_convert_svg(const std::string &filename)
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
    std::vector<std::pair<unsigned, unsigned>> path;
    x = 0;
    y = 0;
    calcomp_parse([&](bool new_path, int dx, int dy) {
        x += dx;
        y += dy;
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
        path.push_back({x - minx, maxy - y});
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
