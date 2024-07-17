//
// Disk unit for BESM-6.
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
#ifndef DUBNA_DISK_H
#define DUBNA_DISK_H

#include "memory.h"

class Disk {
private:
    // Tape name and number.
    Word volume_id;

    // Reference to the BESM-6 memory.
    Memory &memory;

    // File path.
    std::string path;
    bool write_permit;
    int file_descriptor;
    unsigned num_zones;

public:
    // Constructor throws exception if the file cannot be opened.
    Disk(Word id, Memory &memory, const std::string &path, bool write_permit);
    Disk(Word id, Memory &memory, const std::string &pattern, unsigned num_zones);

    // Clone the disk.
    Disk(const Disk &other);

    // Close file in destructor.
    ~Disk();

    // Data transfer.
    void disk_to_memory(unsigned zone, unsigned sector, unsigned addr, unsigned nwords);
    void memory_to_disk(unsigned zone, unsigned sector, unsigned addr, unsigned nwords);

    Word get_id() { return volume_id; }
    const std::string &get_path() { return path; }
};

#endif // DUBNA_DISK_H
