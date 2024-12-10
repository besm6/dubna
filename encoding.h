//
// Encoding routines.
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
#include <iostream>
#include <string>

//
// Write GOST-10859 string to stdout.
// Convert to local encoding UTF-8.
//
void gost_write(const std::string &line, unsigned limit);

//
// Convert character in GOST-10859 encoding to Unicode.
//
unsigned gost_to_unicode(unsigned char ch);

//
// Check for end-of-text symbol.
//
bool is_gost_end_of_text(unsigned char ch);

//
// Write Unicode symbol to stdout in UTF-8 encoding.
//
void utf8_putc(unsigned ch);

//
// Write Unicode symbol to a stream in UTF-8 encoding.
//
void utf8_putc(unsigned ch, std::ostream &s);

//
// Write ISO symbol to a stream in UTF-8 encoding.
//
void iso_putc(unsigned ch, std::ostream &s);

//
// Fetch Unicode symbol from UTF-8 string.
// Advance string pointer.
//
unsigned utf8_to_unicode(const char **p);

//
// Convert Unicode character to KOI-7 encoding.
//
unsigned char unicode_to_koi7(unsigned short val);

//
// Convert string from UTF-8 encoding to KOI-7.
//
std::string utf8_to_koi7(const std::string &input, size_t maxlen = 80);

//
// Convert ITM encoding to/from GOST-10859.
//
extern const unsigned char itm_to_gost[256], gost_to_itm[256];

//
// Convert KOI-7(ISO) encoding to Unicode.
//
extern const unsigned short koi7_to_unicode[128];

//
// Convert character in TEXT encoding to Unicode.
//
unsigned text_to_unicode(unsigned char ch);

//
// ALLTOISO table from Dubna OS kernel.
//
extern const unsigned long long all_to_iso[128];
