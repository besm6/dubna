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
        watanabe_save("watanabe.out");
        watanabe_convert_svg("plotter.svg");
        watanabe.erase();
    }
    // TODO: calcomp
    // TODO: tektronix
}

//
// Save Watanabe output, if available.
//
void Plotter::watanabe_save(const std::string &filename)
{
    std::ofstream out(filename);
    if (!out.is_open()) {
        std::cerr << filename << ": " << std::strerror(errno) << std::endl;
        return;
    }
    out << watanabe;
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
        unsigned int x, y;
        int num_read = std::sscanf(line.c_str(), "%c%u,%u", &ch, &x, &y);
        if (num_read != 3) {
            // Ignore bad line.
            continue;
        }
        func(ch, x, y);
    }
}
