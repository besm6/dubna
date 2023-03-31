//
// Drum unit for BESM-6.
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
#ifndef DUBNA_DRUM_H
#define DUBNA_DRUM_H

#include "memory.h"

class Drum {
private:
    // Reference to the BESM-6 memory.
    Memory &memory;

    // Drum contents.
    std::array<Word, 040 * PAGE_NWORDS> media;

public:
    // Constructor.
    explicit Drum(Memory &m) : memory(m) {}

    // Data transfer.
    void drum_to_memory(unsigned zone, unsigned sector, unsigned addr, unsigned nwords);
    void memory_to_drum(unsigned zone, unsigned sector, unsigned addr, unsigned nwords);

    // Single word access.
    void write_word(unsigned offset, Word value) { media[offset] = value; }
    Word read_word(unsigned offset) { return media[offset]; }
};

#endif // DUBNA_DRUM_H
