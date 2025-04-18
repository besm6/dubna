//
// COSY file encoding.
//
// Copyright (c) 2022-2023 Leonid Broukhis, Serge Vakulenko
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
#include "cosy.h"

#include <filesystem>
#include <fstream>
#include <sstream>

#include "besm6_arch.h"
#include "encoding.h"

//
// Card '*read old' has a special representation.
// Note the space symbol is not packed.
// It is impossible to obtain with a regular encode_cosy() routine.
//
static const std::string COSY_READ_OLD = "*READ OLD\312\n\n";

//
// Card '*end file' has two alternative representations.
//
static const std::string COSY_END_FILE_REGULAR = "*END\201FILE\312\n\n";

//
// Legacy variant produced by *EDIT.
// Note the space symbol is not packed,
// and extra space is appended.
//
static const std::string COSY_END_FILE_LEGACY = "*END FILE \311\n";

//
// Encode string to COSY format.
//
std::string encode_cosy(std::string line)
{
    // Extend to 83 characters and append newline.
    line.append(83 - line.size(), ' ');
    line.append(1, '\n');

    // Pack spaces.
    unsigned num_spaces        = 0;
    unsigned first_space_index = 0;
    unsigned n;
    for (n = 0; n < line.size(); n++) {
        if (line[n] == ' ') {
            // Space.
            if (num_spaces == 0) {
                first_space_index = n;
            }
            num_spaces++;
        } else {
            // Non-space character.
            if (num_spaces > 0) {
                // Replace spaces with packed byte.
                line[first_space_index] = '\200' + num_spaces;
                if (num_spaces > 1) {
                    // Remove spaces.
                    line.erase(first_space_index + 1, num_spaces - 1);
                    n -= num_spaces - 1;
                }
                num_spaces        = 0;
                first_space_index = 0;
            }
        }
    }

    // Align to 6 bytes.
    switch (line.size() % 6) {
    case 1:
        line.append("\40\40\40\40\12");
        break;
    case 2:
        line.append("\40\40\40\12");
        break;
    case 3:
        line.append("\40\40\12");
        break;
    case 4:
        line.append("\40\12");
        break;
    case 5:
        line.append("\12");
        break;
    }
    return line;
}

//
// Create file.bin in COSY format from file.txt.
// Return true when succeeded.
//
bool file_txt_to_cosy(const std::string &path_bin)
{
    if (std::filesystem::exists(path_bin)) {
        // Binary file already exists - refuse to convert.
        return false;
    }

    // Replace '.bin' extension with '.txt'.
    std::filesystem::path path_txt{ path_bin };
    path_txt.replace_extension(".txt");

    // Open text file.
    std::ifstream input(path_txt.string());
    if (!input.good()) {
        // No text file.
        return false;
    }

    // Open binary file for write.
    std::ofstream output(path_bin, std::ios::binary);
    if (!output.good()) {
        // Cannot write.
        return false;
    }

    std::string line;
    while (std::getline(input, line)) {
        line = utf8_to_koi7(line, 80);
        line = encode_cosy(line);
        output << line;
    }
    output << "*READ OLD\312\n\n";
    output << "*END FILE \311\n";

    // Fill the rest of the zone with zeroes.
    uint64_t size    = output.tellp();
    uint64_t aligned = (size + PAGE_NBYTES - 1) / PAGE_NBYTES * PAGE_NBYTES;
    if (size != aligned) {
        output.seekp(aligned - 1);
        output << (char)0;
    }
    return true;
}

//
// Convert file.bin in COSY format to file.txt.
// Return true when succeeded.
//
bool file_cosy_to_txt(const std::string &path_bin)
{
    // Replace '.bin' extension with '.txt'.
    std::filesystem::path path_txt{ path_bin };
    path_txt.replace_extension(".txt");

    // Open binary file.
    std::ifstream input(path_bin, std::ios::binary);
    if (!input.good()) {
        // No binary file.
        return false;
    }

    // Open text file for write.
    std::ofstream output(path_txt.string());
    if (!output.good()) {
        // Cannot write.
        return false;
    }

    std::string line;
    for (;;) {
        if (!get_line_cosy(input, line)) {
            // End of file - card '*read old' is missing.
            std::filesystem::remove(path_txt);
            return false;
        }
        if (is_read_old_cosy(line)) {
            // Valid '*read old' found.
            break;
        }
        if (!decode_cosy(line)) {
            // Bad data.
            std::filesystem::remove(path_txt);
            return false;
        }
        output << line << '\n';
    }

    // Check next card.
    if (!get_line_cosy(input, line)) {
        // Cannot read '*end file' card.
        std::filesystem::remove(path_txt);
        return false;
    }
    if (!is_end_file_cosy(line)) {
        // Card '*end file' is missing.
        std::filesystem::remove(path_txt);
        return false;
    }
    return true;
}

//
// Read COSY card from an input stream and place it into a string.
// Return false on EOF or failure.
//
bool get_line_cosy(std::istream &input, std::string &line)
{
    // Read words up to '\n' in lower byte.
    line.clear();
    do {
        if (line.size() >= 14 * 6) {
            // A valid COSY line can have up to 14 words, or 84 bytes.
            return false;
        }

        // Read next word.
        line.push_back(input.get());
        line.push_back(input.get());
        line.push_back(input.get());
        line.push_back(input.get());
        line.push_back(input.get());
        line.push_back(input.get());
        if (!input.good()) {
            // Premature end of file.
            return false;
        }
    } while (line.back() != '\n');
    return true;
}

//
// Recognize card '*read old'.
//
bool is_read_old_cosy(const std::string &line)
{
    return line == COSY_READ_OLD;
}

//
// Recognize card '*end file'.
// It has two alternative representations.
//
bool is_end_file_cosy(const std::string &line)
{
    return (line == COSY_END_FILE_REGULAR) || (line == COSY_END_FILE_LEGACY);
}

//
// Decode string from COSY format.
// Return false on failure.
//
bool decode_cosy(std::string &line)
{
    std::stringstream result;

    for (uint8_t ch : line) {
        if (ch == '\n') {
            // End of line.
            break;
        }
        if (ch >= 0201 && ch <= 0323) {
            // Unpack spaces.
            while (ch-- > 0200) {
                result << ' ';
            }
            continue;
        }
        if (ch < ' ' || ch > 0177) {
            // Bad byte.
            return false;
        }
        utf8_putc(koi7_to_unicode[ch], result);
    }
    line = result.str();

    // Remove trailing spaces.
    line.erase(line.find_last_not_of(" ") + 1);
    return true;
}

//
// Create file.bin in ISO format from file.txt.
// Return true when succeeded.
//
bool file_utxt_to_iso(const std::string &path_bin)
{
    if (std::filesystem::exists(path_bin)) {
        // Binary file already exists - refuse to convert.
        return false;
    }

    // Replace '.bin' extension with '.utxt'.
    std::filesystem::path path_txt{ path_bin };
    path_txt.replace_extension(".utxt");

    // Open text file.
    std::ifstream input(path_txt.string());
    if (!input.good()) {
        // No text file.
        return false;
    }

    // Open binary file for write.
    std::ofstream output(path_bin, std::ios::binary);
    if (!output.good()) {
        // Cannot write.
        return false;
    }

    std::string line;
    while (std::getline(input, line)) {
        line = utf8_to_koi7(line, 130);
        output << line;
        output << '\n';
    }
    output << "\0\0\0\0\0\0\0\0\0\0\0";

    // Fill the rest of the zone with zeroes.
    uint64_t size    = output.tellp();
    uint64_t aligned = (size + PAGE_NBYTES - 1) / PAGE_NBYTES * PAGE_NBYTES;
    if (size != aligned) {
        output.seekp(aligned - 1);
        output << (char)0;
    }
    return true;
}
