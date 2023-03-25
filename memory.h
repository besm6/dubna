//
// Copyright (c) 2023 Serge Vakulenko
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
#ifndef DUBNA_MEMORY_H
#define DUBNA_MEMORY_H

#include <array>

#include "besm6_arch.h"

class Memory {
private:
    // Memory contents.
    std::array<Word, MEMORY_NWORDS> mem;

public:
    explicit Memory() = default;
    virtual ~Memory() = default;

    // Store data to memory.
    void store_word(const Word &input, unsigned addr);

    // Load data from memory.
    void load_word(Word &output, unsigned addr);

    // Debug access to memory: no tracing.
    void debug_write(const Words &input, unsigned addr);
    void debug_read(Words &output, unsigned nrows, unsigned addr);

    // Cannot copy the Memory object.
    Memory(const Memory &) = delete;
    Memory &operator=(const Memory &) = delete;
};

#endif // DUBNA_MEMORY_H
