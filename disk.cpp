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
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cstring>
#include <iostream>

#include "machine.h"

//
// Open binary image as disk.
//
Disk::Disk(Word id, Memory &m, const std::string &p, bool wp)
  : volume_id(id), memory(m), path(p), write_permit(wp)
{
    // Open file.
    int open_flag   = write_permit ? O_RDWR : O_RDONLY;
    file_descriptor = open(path.c_str(), open_flag);
    if (file_descriptor < 0)
        throw std::runtime_error("Cannot open " + path +
                                 (write_permit ? " for write" : " for read"));

    // Get file size.
    struct stat stat;
    fstat(file_descriptor, &stat);
    num_zones = stat.st_size / DISK_ZONE_NWORDS;

    if (num_zones == 0 && volume_id == 0 && write_permit) {
        // Empty file mounted for write by *FILE card.
        // Assume size of 1024 zones.
        num_zones = 02000;
    }
}

//
// Open temporary file as disk.
//
Disk::Disk(Word id, Memory &m, const std::string &p, unsigned nz)
  : volume_id(id), memory(m), path(p + ".@XXXXXX"), write_permit(true), num_zones(nz)
{
    // Create a unique file with name derived from template.
    file_descriptor = mkstemp(&path[0]);
    if (file_descriptor < 0)
        throw std::runtime_error("Cannot create " + path);
}

//
// Clone the disk.
//
Disk::Disk(const Disk &other)
  : volume_id(other.volume_id), memory(other.memory), path(other.path),
    write_permit(other.write_permit), num_zones(other.num_zones)
{
    // Duplicate the file descriptor.
    file_descriptor = dup(other.file_descriptor);
    if (file_descriptor < 0) {
        throw std::runtime_error("Cannot duplicate descriptor for " + other.path);
    }
}

// Close file in destructor.
Disk::~Disk()
{
    close(file_descriptor);
}

//
// Disk read: transfer data to memory.
//
void Disk::disk_to_memory(unsigned zone, unsigned sector, unsigned addr, unsigned nwords)
{
    zone += DISK_ZONE_OFFSET;
    if (zone >= num_zones)
        throw std::runtime_error("Zone number exceeds disk size");

    unsigned offset_nwords = (DISK_ZONE_NWORDS * zone) + // start of the zone record
                             8 +                         // skip OS info
                             (256 * sector);             // sector offset

    if (lseek(file_descriptor, offset_nwords * sizeof(Word), SEEK_SET) < 0)
        throw std::runtime_error("Disk seek error");

    Word *destination = memory.get_ptr(addr);
    unsigned nbytes   = nwords * sizeof(Word);
    int nread         = read(file_descriptor, destination, nbytes);

    if (nread == 0) {
        // Read past the end of file - return zeroes.
        std::memset(destination, 0, nbytes);
    } else if (nread != (int)nbytes) {
        throw std::runtime_error("Disk read error");
    }
}

//
// Disk write: transfer data from memory.
//
void Disk::memory_to_disk(unsigned zone, unsigned sector, unsigned addr, unsigned nwords)
{
    if (!write_permit)
        throw std::runtime_error("Cannot write to read-only disk");

    zone += DISK_ZONE_OFFSET;
    if (zone >= num_zones)
        throw std::runtime_error("Zone number exceeds disk size");

    unsigned offset_nwords = (DISK_ZONE_NWORDS * zone) + // start of the zone record
                             8 +                         // skip OS info
                             (256 * sector);             // sector offset

    if (lseek(file_descriptor, offset_nwords * sizeof(Word), SEEK_SET) < 0)
        throw std::runtime_error("Disk seek error");

    Word *source    = memory.get_ptr(addr);
    unsigned nbytes = nwords * sizeof(Word);
    if (write(file_descriptor, source, nbytes) != nbytes)
        throw std::runtime_error("Disk write error");
}
