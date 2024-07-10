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

#include "machine.h"

//
// Finish operation.
// Save all data files.
//
Plotter::~Plotter()
{
    watanabe_save();
    // TODO: calcomp
    // TODO: tektronix
}

//
// Save Watanabe output, if available.
//
void Plotter::watanabe_save()
{
    if (watanabe.empty())
        return;

    // Save raw output to this filename.
    const std::string filename = "watanabe.out";

    std::ofstream out(filename);
    if (!out.is_open()) {
        std::cerr << filename << ": " << std::strerror(errno) << std::endl;
        return;
    }
    out << watanabe;
}
