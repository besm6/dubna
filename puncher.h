//
// Punched card output for BESM-6.
//
// Copyright (c) 2024 Leonid Broukhis
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
#ifndef DUBNA_PUNCHER_H
#define DUBNA_PUNCHER_H

#include <iostream>
#include <fstream>
#include "memory.h"

class Puncher {
private:
    Memory& memory;
    std::ofstream braille;
    std::ofstream stdarray;

public:
    Puncher(Memory& m) : memory(m) { }

    // Send an array to punched cards.
    void punch(ushort start_addr, ushort end_addr);

private:
    void punch_braille(unsigned char buf[144]);
    void punch_stdarray(unsigned char buf[144]);
};

#endif // DUBNA_PLOTTER_H
