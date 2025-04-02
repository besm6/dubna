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
#include <string>

//
// Create file.bin in COSY format from file.txt.
// Return true when succeeded.
//
bool file_txt_to_cosy(const std::string &path_bin);

//
// Convert file.bin in COSY format to file.txt.
// Return true when succeeded.
//
bool file_cosy_to_txt(const std::string &path_bin);

//
// Create file.bin in ISO format from file.txt.
// Return true when succeeded.
//
bool file_utxt_to_iso(const std::string &path_bin);

//
// Encode string to COSY format.
//
std::string encode_cosy(std::string line);

//
// Decode string from COSY format.
// Return false on failure.
//
bool decode_cosy(std::string &line);

//
// Read COSY card from an input stream and place it into a string.
// Return false on EOF or failure.
//
bool get_line_cosy(std::istream &input, std::string &line);

//
// Recognize card '*read old'.
//
bool is_read_old_cosy(const std::string &line);

//
// Recognize card '*end file'.
//
bool is_end_file_cosy(const std::string &line);
