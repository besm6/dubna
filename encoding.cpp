//
// Text encoding for BESM-6.
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
#include "encoding.h"

#include <iostream>

#include "gost10859.h"

static const bool GOST_LATIN = true; // default latin

//
// GOST-10859 encoding.
// Documentation: http://en.wikipedia.org/wiki/GOST_10859
//
static const unsigned short gost_to_unicode_cyr[256] = {
    /* 000-007 */ 0x30,   0x31,   0x32,   0x33,   0x34,   0x35,   0x36,   0x37,
    /* 010-017 */ 0x38,   0x39,   0x2b,   0x2d,   0x2f,   0x2c,   0x2e,   0x20,
    /* 020-027 */ 0x23e8, 0x2191, 0x28,   0x29,   0xd7,   0x3d,   0x3b,   0x5b,
    /* 030-037 */ 0x5d,   0x2a,   0x2018, 0x2019, 0x2260, 0x3c,   0x3e,   0x3a,
    /* 040-047 */ 0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417,
    /* 050-057 */ 0x0418, 0x0419, 0x041a, 0x041b, 0x041c, 0x041d, 0x041e, 0x041f,
    /* 060-067 */ 0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427,
    /* 070-077 */ 0x0428, 0x0429, 0x042b, 0x042c, 0x042d, 0x042e, 0x042f, 0x44,
    /* 100-107 */ 0x46,   0x47,   0x49,   0x4a,   0x4c,   0x4e,   0x51,   0x52,
    /* 110-117 */ 0x53,   0x55,   0x56,   0x57,   0x5a,   0x203e, 0x2a7d, 0x2a7e,
    /* 120-127 */ 0x2228, 0x2227, 0x2283, 0xac,   0xf7,   0x2261, 0x25,   0x25c7,
    /* 130-137 */ 0x7c,   0x2015, 0x5f,   0x21,   0x22,   0x042a, 0xb0,   0x2032,
};

static const unsigned short gost_to_unicode_lat[256] = {
    /* 000-007 */ 0x30,   0x31,   0x32,   0x33,   0x34,   0x35,   0x36,   0x37,
    /* 010-017 */ 0x38,   0x39,   0x2b,   0x2d,   0x2f,   0x2c,   0x2e,   0x20,
    /* 020-027 */ 0x23e8, 0x2191, 0x28,   0x29,   0xd7,   0x3d,   0x3b,   0x5b,
    /* 030-037 */ 0x5d,   0x2a,   0x2018, 0x2019, 0x2260, 0x3c,   0x3e,   0x3a,
    /* 040-047 */ 0x41,   0x0411, 0x42,   0x0413, 0x0414, 0x45,   0x0416, 0x0417,
    /* 050-057 */ 0x0418, 0x0419, 0x4b,   0x041b, 0x4d,   0x48,   0x4f,   0x041f,
    /* 060-067 */ 0x50,   0x43,   0x54,   0x59,   0x0424, 0x58,   0x0426, 0x0427,
    /* 070-077 */ 0x0428, 0x0429, 0x042b, 0x042c, 0x042d, 0x042e, 0x042f, 0x44,
    /* 100-107 */ 0x46,   0x47,   0x49,   0x4a,   0x4c,   0x4e,   0x51,   0x52,
    /* 110-117 */ 0x53,   0x55,   0x56,   0x57,   0x5a,   0x203e, 0x2a7d, 0x2a7e,
    /* 120-127 */ 0x2228, 0x2227, 0x2283, 0xac,   0xf7,   0x2261, 0x25,   0x25c7,
    /* 130-137 */ 0x7c,   0x2015, 0x5f,   0x21,   0x22,   0x042a, 0xb0,   0x2032,
};

//
// Convert character in GOST-10859 encoding to Unicode.
//
unsigned gost_to_unicode(unsigned char ch)
{
    return GOST_LATIN ? gost_to_unicode_lat[ch] : gost_to_unicode_cyr[ch];
}

//
// Write GOST-10859 string to stdout.
// Convert to local encoding UTF-8.
//
void gost_write(const std::string &line, unsigned limit)
{
    for (unsigned i = 0; i <= limit; i++) {
        unsigned ch = gost_to_unicode(line[i]);
        if (!ch)
            ch = ' ';
        utf8_putc(ch);
    }
}

//
// Check for end-of-text symbol.
//
bool is_gost_end_of_text(unsigned char ch)
{
    switch (ch) {
    case GOST_EOF:
    case GOST_END_OF_INFORMATION:
    case 0231:
        return true;
    default:
        return false;
    }
}

//
// Write Unicode symbol to stdout.
//
void utf8_putc(unsigned ch)
{
#if 0
    static int initialized = 0;

    if (!initialized) {
        // Write UTF-8 tag: zero width no-break space.
        std::cout << (char)0xEF;
        std::cout << (char)0xBB;
        std::cout << (char)0xBF;
        initialized = 1;
    }
#endif
    utf8_putc(ch, std::cout);
}

//
// Write Unicode symbol to a stream.
// Convert to UTF-8 encoding:
// 00000000.0xxxxxxx -> 0xxxxxxx
// 00000xxx.xxyyyyyy -> 110xxxxx, 10yyyyyy
// xxxxyyyy.yyzzzzzz -> 1110xxxx, 10yyyyyy, 10zzzzzz
//
void utf8_putc(unsigned ch, std::ostream &s)
{
    if (ch < 0x80) {
        s << (char)ch;
        return;
    }
    if (ch < 0x800) {
        s << (char)(ch >> 6 | 0xc0);
        s << (char)((ch & 0x3f) | 0x80);
        return;
    }
    s << (char)(ch >> 12 | 0xe0);
    s << (char)(((ch >> 6) & 0x3f) | 0x80);
    s << (char)((ch & 0x3f) | 0x80);
}

//
// Fetch Unicode symbol from UTF-8 string.
// Advance string pointer.
//
unsigned utf8_to_unicode(const char **p)
{
    unsigned c1, c2, c3;

    c1 = (unsigned char)*(*p)++;
    if (!(c1 & 0x80))
        return c1;
    c2 = (unsigned char)*(*p)++;
    if (!(c1 & 0x20))
        return (c1 & 0x1f) << 6 | (c2 & 0x3f);
    c3 = (unsigned char)*(*p)++;
    return (c1 & 0x0f) << 12 | (c2 & 0x3f) << 6 | (c3 & 0x3f);
}

//
// Write ISO symbol to a stream in UTF-8 encoding.
//
void iso_putc(unsigned ch, std::ostream &out)
{
    utf8_putc(koi7_to_unicode[ch], out);
}

//
// Convert Unicode character to KOI7 encoding.
// For details, see:
// https://ru.wikipedia.org/wiki/%D0%9A%D0%9E%D0%98-7#%D0%9A%D0%9E%D0%98-7_%D0%9D2
//
unsigned char unicode_to_koi7(unsigned short val)
{
    static const unsigned char tab0[256] = {
        // clang-format off
        /* 00 - 07 */  0,    0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
        /* 08 - 0f */  0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
        /* 10 - 17 */  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
        /* 18 - 1f */  0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
        /*  !"#$%&' */ 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
        /* ()*+,-./ */ 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
        /* 01234567 */ 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
        /* 89:;<=>? */ 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
        /* @ABCDEFG */ 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
        /* HIJKLMNO */ 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
        /* PQRSTUVW */ 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
        /* XYZ[\]^_ */ 0x58, 0x59, 0x5a, 0x5b, 0x1d, 0x5d, 0x5c, 0x5f,
        /* `abcdefg */ 0,    0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, //  ABCDEFG
        /* hijklmno */ 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, // HIJKLMNO
        /* pqrstuvw */ 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, // PQRSTUVW
        /* xyz{|}~  */ 0x58, 0x59, 0x5a, 0x0e, 0x5e, 0x0f, 0x1f, 0,    // XYZ⩽|⩾¬
        /* 80 - 87 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* 88 - 8f */  0,    0,    0,    0,    0,    0,    0,    0,
        /* 90 - 97 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* 98 - 9f */  0,    0,    0,    0,    0,    0,    0,    0,
        /* a0 - a7 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* a8 - af */  0,    0,    0,    0,    0x1f, 0,    0,    0, // ¬
        /* b0 - b7 */  0x19, 0,    0,    0,    0,    0,    0,    0, // °
        /* b8 - bf */  0,    0,    0,    0,    0,    0,    0,    0,
        /* c0 - c7 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* c8 - cf */  0,    0,    0,    0,    0,    0,    0,    0,
        /* d0 - d7 */  0,    0,    0,    0,    0,    0,    0,    0x06, // ×
        /* d8 - df */  0,    0,    0,    0,    0,    0,    0,    0,
        /* e0 - e7 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* e8 - ef */  0,    0,    0,    0,    0,    0,    0,    0,
        /* f0 - f7 */  0,    0,    0,    0,    0,    0,    0,    0x1a, // ÷
        /* f8 - ff */  0,    0,    0,    0,    0,    0,    0,    0,
        // clang-format on
    };
    switch (val >> 8) {
    case 0x00:
        return tab0[val];
    case 0x04:
        switch ((unsigned char)val) {
        case 0x01:
            return 'E'; // Ë - 0x65
        case 0x04:
            return 'E'; // Ukrainian Є -> E - 0x65
        case 0x06:
            return 'I'; // Ukrainian І -> I - 0x73
        case 0x07:
            return 'I'; // Ukrainian Ї -> I - 0x73
        case 0x10:
            return 'A'; // А - 0x61
        case 0x11:
            return 0x62; // Б
        case 0x12:
            return 'B'; // В - 0x77
        case 0x13:
            return 0x67; // Г
        case 0x14:
            return 0x64; // Д
        case 0x15:
            return 'E'; // Е - 0x65
        case 0x16:
            return 0x76; // Ж
        case 0x17:
            return 0x7a; // З
        case 0x18:
            return 0x69; // И
        case 0x19:
            return 0x6a; // Й
        case 0x1a:
            return 'K'; // К - 0x6b
        case 0x1b:
            return 0x6c; // Л
        case 0x1c:
            return 'M'; // М - 0x6d
        case 0x1d:
            return 'H'; // Н - 0x6e
        case 0x1e:
            return 'O'; // О - 0x6f
        case 0x1f:
            return 0x70; // П
        case 0x20:
            return 'P'; // Р - 0x72
        case 0x21:
            return 'C'; // С - 0x73
        case 0x22:
            return 'T'; // Т - 0x74
        case 0x23:
            return 'Y'; // У - 0x75
        case 0x24:
            return 0x66; // Ф
        case 0x25:
            return 'X'; // Х - 0x68
        case 0x26:
            return 0x63; // Ц
        case 0x27:
            return 0x7e; // Ч
        case 0x28:
            return 0x7b; // Ш
        case 0x29:
            return 0x7d; // Щ
        case 0x2a:
            return 0x05; // Ъ
        case 0x2b:
            return 0x79; // Ы
        case 0x2c:
            return 0x78; // Ь
        case 0x2d:
            return 0x7c; // Э
        case 0x2e:
            return 0x60; // Ю
        case 0x2f:
            return 0x71; // Я
        case 0x30:
            return 'A'; // а - 0x61
        case 0x31:
            return 0x62; // б
        case 0x32:
            return 'B'; // в - 0x77
        case 0x33:
            return 0x67; // г
        case 0x34:
            return 0x64; // д
        case 0x35:
            return 'E'; // е - 0x65
        case 0x36:
            return 0x76; // ж
        case 0x37:
            return 0x7a; // з
        case 0x38:
            return 0x69; // и
        case 0x39:
            return 0x6a; // й
        case 0x3a:
            return 'K'; // к - 0x6b
        case 0x3b:
            return 0x6c; // л
        case 0x3c:
            return 'M'; // м - 0x6d
        case 0x3d:
            return 'H'; // н - 0x6e
        case 0x3e:
            return 'O'; // о - 0x6f
        case 0x3f:
            return 0x70; // п
        case 0x40:
            return 'P'; // р - 0x72
        case 0x41:
            return 'C'; // с - 0x73
        case 0x42:
            return 'T'; // т - 0x74
        case 0x43:
            return 'Y'; // у - 0x75
        case 0x44:
            return 0x66; // ф
        case 0x45:
            return 'X'; // х - 0x68
        case 0x46:
            return 0x63; // ц
        case 0x47:
            return 0x7e; // ч
        case 0x48:
            return 0x7b; // ш
        case 0x49:
            return 0x7d; // щ
        case 0x4a:
            return 0x05; // ъ
        case 0x4b:
            return 0x79; // ы
        case 0x4c:
            return 0x78; // ь
        case 0x4d:
            return 0x7c; // э
        case 0x4e:
            return 0x60; // ю
        case 0x4f:
            return 0x71; // я
        case 0x51:
            return 'E'; // ё - 0x65
        case 0x54:
            return 'E'; // Ukrainian є -> е - 0x65
        case 0x56:
            return 'I'; // Ukrainian і -> i - 0x73
        case 0x57:
            return 'I'; // Ukrainian ї -> i - 0x73
        case 0x90:
            return 0x67; // Ukrainian Ґ -> Г
        case 0x91:
            return 0x67; // Ukrainian ґ -> г
        }
        break;
    case 0x20:
        switch ((unsigned char)val) {
        case 0x15:
            return '\25'; // ―
        case 0x18:
            return '\20'; // ‘
        case 0x19:
            return '\33'; // ’
        case 0x28:
            return 0x0a;
        case 0x32:
            return '\'';
        case 0x3e:
            return '\\';
        }
        break;
    case 0x21:
        switch ((unsigned char)val) {
        case 0x2f:
            return 'E';
        case 0x91:
            return '\26'; // ↑ (was '@')
        }
        break;
    case 0x22:
        switch ((unsigned char)val) {
        case 0x27:
            return '&'; // ∧
        case 0x28:
            return '\36'; // ∨
        case 0x60:
            return '\30'; // ≠ (was '#')
        case 0x61:
            return '\35'; // ≡
        case 0x64:
            return '\16'; // ≤
        case 0x65:
            return '\17'; // ≥
        case 0x83:
            return '\34'; // ⊃
        }
        break;
    case 0x23:
        switch ((unsigned char)val) {
        case 0xe8:
            return '\27'; // ⏨
        }
        break;
    case 0x25:
        switch ((unsigned char)val) {
        case 0xc7:
            return '$';
        }
        break;
    case 0x2a:
        switch ((unsigned char)val) {
        case 0x7d:
            return '\16'; // ⩽
        case 0x7e:
            return '\17'; // ⩾
        }
        break;
    }
    return 0;
}

//
// Convert string from UTF-8 encoding to KOI-7.
//
std::string utf8_to_koi7(const std::string &input, size_t maxlen)
{
    const char *ptr = input.c_str();
    std::string line;
    while (line.size() < maxlen) {
        // Get unicode character.
        unsigned u = utf8_to_unicode(&ptr);
        if (!u)
            break;

        // Convert to KOI-7.
        unsigned ch = unicode_to_koi7(u);
        if (!ch)
            continue;

        line.append(1, ch);
    }
    return line;
}

/*
 * Encoding of ITM autocode.
 * Documentation:
 * https://github.com/besm6/besm6.github.io/raw/master/doc/%D0%90%D0%B2%D1%82%D0%BE%D0%BA%D0%BE%D0%B4-%D0%A7%D0%B0%D0%B9%D0%BA%D0%BE%D0%B2%D1%81%D0%BA%D0%BE%D0%B3%D0%BE.pdf
 */
const unsigned char itm_to_gost[256] = {
    // clang-format off
    /* 000 */ GOST_0,                GOST_1,
              GOST_2,                GOST_3,
              GOST_4,                GOST_5,
              GOST_6,                GOST_7,
    /* 010 */ GOST_8,                GOST_9,
              0,                     0,
              0,                     0,
              0,                     GOST_SPACE,
    /* 020 */ 0,                     0,
              0,                     0,
              0,                     0,
              0,                     0,
    /* 030 */ 0,                     0,
              0,                     0,
              0,                     0,
              0,                     0,
    /* 040 */ GOST_SPACE,            GOST_RIGHT_QUOTATION,
              GOST_DIAMOND,          GOST_UNDERLINE,
              GOST_VERTICAL_LINE,    GOST_SEMICOLON,
              GOST_COMMA,            GOST_DOT,
    /* 050 */ GOST_OVERLINE,         GOST_RIGHT_PARENTHESIS,
              0,                     GOST_LEFT_BRACKET,
              GOST_GREATER_THAN,     GOST_DEGREE,
              GOST_COLON,            GOST_EQUALS,
    /* 060 */ GOST_V,                GOST_PLUS,
              GOST_PERCENT,          GOST_EXCLAMATION,
              GOST_LEFT_QUOTATION,   GOST_COLON,
              GOST_RIGHT_BRACKET,    GOST_SLASH,
    /* 070 */ GOST_MINUS,            GOST_LOGICAL_AND,
              GOST_X,                0,
              GOST_LESS_THAN,        GOST_LEFT_QUOTATION,
              GOST_LEFT_PARENTHESIS, 0,
    /* 100 */ 0,                     0,
              0,                     0,
              0,                     0,
              0,                     0,
    /* 110 */ 0,                     0,
              0,                     0,
              0,                     0,
              0,                     0,
    /* 120 */ 0,                     0,
              0,                     0,
              0,                     0,
              0,                     0,
    /* 130 */ 0,                     0,
              0,                     0,
              GOST_QUOTATION,        GOST_HARD_SIGN,
              0,                     GOST_RIGHT_QUOTATION,
    /* 140 */ 0,                     0,
              0,                     GOST_UPWARDS_ARROW,
              0,                     GOST_NOT,
              GOST_LESS_THAN,        GOST_GREATER_THAN,
    /* 150 */ GOST_MINUS,            0,
              GOST_NOT_EQUAL_TO,     0,
              0,                     0,
              0,                     0,
    /* 160 */ 0,                     0,
              0,                     0,
              0,                     0,
              0,                     0,
    /* 170 */ GOST_ASTERISK,         0,
              0,                     0,
              0,                     0,
              GOST_E,                0,
    /* 200 */ 0,                     GOST_T,
              0,                     GOST_O,
              0,                     GOST_H,
              GOST_N,                GOST_M,
    /* 210 */ 0,                     GOST_L,
              GOST_R,                GOST_G,
              GOST_I,                GOST_P,
              GOST_C,                GOST_V,
    /* 220 */ GOST_E,                GOST_Z,
              GOST_D,                GOST_B,
              GOST_S,                GOST_Y,
              GOST_F,                GOST_X,
    /* 230 */ GOST_A,                GOST_W,
              GOST_J,                0,
              GOST_U,                GOST_Q,
              GOST_K,                0,
    /* 240 */ 0,                     0,
              0,                     0,
              0,                     0,
              0,                     0,
    /* 250 */ 0,                     0,
              0,                     0,
              0,                     0,
              0,                     0,
    /* 260 */ 0,                     0,
              0,                     0,
              0,                     0,
              0,                     0,
    /* 270 */ 0,                     0,
              0,                     0,
              0,                     0,
              0,                     0,
    /* 300 */ 0,                     GOST_SHCHA,
              0,                     0,
              0,                     0,
              0,                     0,
    /* 310 */ 0,                     GOST_EL,
              0,                     GOST_GHE,
              GOST_CYRILLIC_I,       GOST_PE,
              GOST_TSE,              GOST_ZHE,
    /* 320 */ GOST_REVERSE_E,        GOST_ZE,
              GOST_DE,               GOST_BE,
              GOST_SHA,              GOST_YERU,
              GOST_EF,               GOST_SOFT_SIGN,
    /* 330 */ 0,                     GOST_CHE,
              GOST_SHORT_I,          0,
              GOST_YU,               GOST_YA,
              0,                     0,
    /* 340 */ 0,                     0,
              0,                     0,
              0,                     0,
              0,                     0,
    /* 350 */ 0,                     0,
              0,                     0,
              0,                     0,
              0,                     0,
    /* 360 */ 0,                     0,
              0,                     0,
              0,                     0,
              0,                     0,
    /* 370 */ 0,                     0,
              0,                     0,
              0,                     0,
              0,                     0,
    // clang-format on
};

const unsigned char gost_to_itm[256] = {
    /* 000-007 */ 0000, 0001, 0002, 0003, 0004, 0005, 0006, 0007,
    /* 010-017 */ 0010, 0011, 0061, 0070, 0067, 0046, 0047, 0017,
    /* 020-027 */ 0220, 0143, 0076, 0051, 0227, 0057, 0045, 0053,
    /* 030-037 */ 0066, 0170, 0064, 0041, 0152, 0074, 0054, 0056,
    /* 040-047 */ 0230, 0323, 0223, 0313, 0322, 0220, 0317, 0321,
    /* 050-057 */ 0314, 0332, 0236, 0311, 0207, 0205, 0203, 0315,
    /* 060-067 */ 0215, 0216, 0201, 0225, 0326, 0227, 0316, 0331,
    /* 070-077 */ 0324, 0301, 0325, 0327, 0320, 0334, 0335, 0222,
    /* 100-107 */ 0226, 0213, 0214, 0232, 0211, 0206, 0235, 0212,
    /* 110-117 */ 0224, 0234, 0217, 0231, 0221, 0050, 0000, 0000,
    /* 120-127 */ 0217, 0071, 0055, 0145, 0000, 0057, 0062, 0042,
    /* 130-137 */ 0044, 0070, 0043, 0063, 0134, 0136, 0064, 0041,
};

/*
 * "Text" encoding of monitoring system Dubna.
 */
const unsigned char text_to_gost[64] = {
    /* 000 */ GOST_SPACE,
    GOST_DOT,
    GOST_BE,
    GOST_TSE,
    GOST_DE,
    GOST_EF,
    GOST_GHE,
    GOST_CYRILLIC_I,
    /* 010 */ GOST_LEFT_PARENTHESIS,
    GOST_RIGHT_PARENTHESIS,
    GOST_ASTERISK,
    GOST_SHORT_I,
    GOST_EL,
    GOST_YA,
    GOST_ZHE,
    GOST_SLASH,
    /* 020 */ GOST_0,
    GOST_1,
    GOST_2,
    GOST_3,
    GOST_4,
    GOST_5,
    GOST_6,
    GOST_7,
    /* 030 */ GOST_8,
    GOST_9,
    GOST_SOFT_SIGN,
    GOST_COMMA,
    GOST_PE,
    GOST_MINUS,
    GOST_PLUS,
    GOST_YERU,
    /* 040 */ GOST_ZE,
    GOST_A,
    GOST_B,
    GOST_C,
    GOST_D,
    GOST_E,
    GOST_F,
    GOST_G,
    /* 050 */ GOST_H,
    GOST_I,
    GOST_J,
    GOST_K,
    GOST_L,
    GOST_M,
    GOST_N,
    GOST_O,
    /* 060 */ GOST_P,
    GOST_Q,
    GOST_R,
    GOST_S,
    GOST_T,
    GOST_U,
    GOST_V,
    GOST_W,
    /* 070 */ GOST_X,
    GOST_Y,
    GOST_Z,
    GOST_SHA,
    GOST_REVERSE_E,
    GOST_SHCHA,
    GOST_CHE,
    GOST_YU,
};

const unsigned short koi7_to_unicode[128] = {
    // clang-format off
    /*      Ъ×  */ 0,      0x01,   0x02,   0x03,   0x04,   0x042a, 0x06,   0xd7,
    /*       ⩽⩾ */ 0x08,   0x09,   0x0a,   0x0b,   0x0c,   0x0d,   0x2a7d, 0x2a7e,
    /* ‘    ―↑⏨ */ 0x2018, 0x11,   0x12,   0x13,   0x14,   0x2015, 0x2191, 0x23e8,
    /* ≠°÷’⊃≡∨¬ */ 0x2260, 0xb0,   0xf7,   0x2019, 0x2283, 0x2261, 0x2228, 0xac,
    /*  !"#$%&' */ 0x20,   0x21,   0x22,   0x23,   0x24,   0x25,   0x26,   0x27,
    /* ()*+,-./ */ 0x28,   0x29,   0x2a,   0x2b,   0x2c,   0x2d,   0x2e,   0x2f,
    /* 01234567 */ 0x30,   0x31,   0x32,   0x33,   0x34,   0x35,   0x36,   0x37,
    /* 89:;<=>? */ 0x38,   0x39,   0x3a,   0x3b,   0x3c,   0x3d,   0x3e,   0x3f,
    /* @ABCDEFG */ 0x40,   0x41,   0x42,   0x43,   0x44,   0x45,   0x46,   0x47,
    /* HIJKLMNO */ 0x48,   0x49,   0x4a,   0x4b,   0x4c,   0x4d,   0x4e,   0x4f,
    /* PQRSTUVW */ 0x50,   0x51,   0x52,   0x53,   0x54,   0x55,   0x56,   0x57,
    /* XYZ[‾]|_ */ 0x58,   0x59,   0x5a,   0x5b,   0x203e, 0x5d,   0x7c,   0x5f,
    /* ЮAБЦДEФГ */ 0x042e, 0x0410, 0x0411, 0x0426, 0x0414, 0x0415, 0x0424, 0x0413,
    /* XИЙKЛMHO */ 0x0425, 0x0418, 0x0419, 0x041a, 0x041b, 0x041c, 0x041d, 0x041e,
    /* ПЯPCTYЖB */ 0x041f, 0x042f, 0x0420, 0x0421, 0x0422, 0x0423, 0x0416, 0x0412,
    /* ЬЫЗШЭЩЧ  */ 0x042c, 0x042b, 0x0417, 0x0428, 0x042d, 0x0429, 0x0427, 0x7f,
    // clang-format on
};

//
// Convert character in TEXT encoding to Unicode.
//
unsigned text_to_unicode(unsigned char ch)
{
    return gost_to_unicode(text_to_gost[ch & 077]);
}

//
// ALLTOISO table from Dubna OS kernel.
//
const unsigned long long all_to_iso[128] = {
    //  GOST          ISO           TEL        CONS.      TEXT      ISO rus
    //  to            to            to         to         to        to
    //  EBCDIC        GOST          ISO        ISO        ISO       ISO lat
    0360ull << 40 | 0017ull << 32 | 0000u << 24 | 0060 << 16 | 0000 << 8 | 0000, // 000
    0361ull << 40 | 0017ull << 32 | 0133u << 24 | 0061 << 16 | 0040 << 8 | 0001, // 001
    0362ull << 40 | 0017ull << 32 | 0176u << 24 | 0062 << 16 | 0142 << 8 | 0002, // 002
    0363ull << 40 | 0017ull << 32 | 0000u << 24 | 0063 << 16 | 0143 << 8 | 0003, // 003
    0364ull << 40 | 0017ull << 32 | 0000u << 24 | 0064 << 16 | 0144 << 8 | 0004, // 004
    0365ull << 40 | 0135ull << 32 | 0000u << 24 | 0065 << 16 | 0146 << 8 | 0005, // 005
    0366ull << 40 | 0024ull << 32 | 0000u << 24 | 0066 << 16 | 0147 << 8 | 0006, // 006
    0367ull << 40 | 0017ull << 32 | 0000u << 24 | 0067 << 16 | 0151 << 8 | 0007, // 007
    0370ull << 40 | 0017ull << 32 | 0000u << 24 | 0070 << 16 | 0056 << 8 | 0010, // 010
    0371ull << 40 | 0017ull << 32 | 0044u << 24 | 0071 << 16 | 0000 << 8 | 0011, // 011
    0116ull << 40 | 0017ull << 32 | 0000u << 24 | 0053 << 16 | 0052 << 8 | 0012, // 012
    0140ull << 40 | 0017ull << 32 | 0000u << 24 | 0055 << 16 | 0152 << 8 | 0013, // 013
    0141ull << 40 | 0017ull << 32 | 0041u << 24 | 0057 << 16 | 0154 << 8 | 0014, // 014
    0153ull << 40 | 0017ull << 32 | 0000u << 24 | 0000 << 16 | 0161 << 8 | 0015, // 015
    0113ull << 40 | 0116ull << 32 | 0000u << 24 | 0000 << 16 | 0166 << 8 | 0016, // 016
    0100ull << 40 | 0117ull << 32 | 0000u << 24 | 0000 << 16 | 0057 << 8 | 0017, // 017
    0305ull << 40 | 0033ull << 32 | 0052u << 24 | 0015 << 16 | 0060 << 8 | 0020, // 020
    0157ull << 40 | 0017ull << 32 | 0000u << 24 | 0012 << 16 | 0061 << 8 | 0021, // 021
    0115ull << 40 | 0017ull << 32 | 0135u << 24 | 0036 << 16 | 0062 << 8 | 0022, // 022
    0135ull << 40 | 0017ull << 32 | 0000u << 24 | 0030 << 16 | 0063 << 8 | 0023, // 023
    0134ull << 40 | 0017ull << 32 | 0000u << 24 | 0000 << 16 | 0064 << 8 | 0024, // 024
    0176ull << 40 | 0017ull << 32 | 0176u << 24 | 0000 << 16 | 0065 << 8 | 0025, // 025
    0136ull << 40 | 0021ull << 32 | 0135u << 24 | 0000 << 16 | 0066 << 8 | 0026, // 026
    0112ull << 40 | 0020ull << 32 | 0000u << 24 | 0040 << 16 | 0067 << 8 | 0027, // 027
    0132ull << 40 | 0034ull << 32 | 0000u << 24 | 0136 << 16 | 0070 << 8 | 0030, // 030
    0134ull << 40 | 0136ull << 32 | 0000u << 24 | 0052 << 16 | 0071 << 8 | 0031, // 031
    0175ull << 40 | 0124ull << 32 | 0000u << 24 | 0000 << 16 | 0170 << 8 | 0032, // 032
    0175ull << 40 | 0032ull << 32 | 0000u << 24 | 0000 << 16 | 0072 << 8 | 0033, // 033
    0173ull << 40 | 0122ull << 32 | 0000u << 24 | 0000 << 16 | 0160 << 8 | 0034, // 034
    0114ull << 40 | 0125ull << 32 | 0000u << 24 | 0000 << 16 | 0055 << 8 | 0035, // 035
    0156ull << 40 | 0120ull << 32 | 0000u << 24 | 0021 << 16 | 0053 << 8 | 0036, // 036
    0172ull << 40 | 0123ull << 32 | 0000u << 24 | 0024 << 16 | 0171 << 8 | 0037, // 037
    0301ull << 40 | 0017ull << 32 | 0240u << 24 | 0101 << 16 | 0172 << 8 | 0040, // 040
    0272ull << 40 | 0133ull << 32 | 0113u << 24 | 0142 << 16 | 0101 << 8 | 0041, // 041
    0302ull << 40 | 0134ull << 32 | 0121u << 24 | 0102 << 16 | 0102 << 8 | 0042, // 042
    0277ull << 40 | 0034ull << 32 | 0125u << 24 | 0147 << 16 | 0103 << 8 | 0043, // 043
    0274ull << 40 | 0127ull << 32 | 0340u << 24 | 0144 << 16 | 0104 << 8 | 0044, // 044
    0305ull << 40 | 0126ull << 32 | 0112u << 24 | 0105 << 16 | 0105 << 8 | 0045, // 045
    0354ull << 40 | 0121ull << 32 | 0127u << 24 | 0166 << 16 | 0106 << 8 | 0046, // 046
    0372ull << 40 | 0032ull << 32 | 0101u << 24 | 0172 << 16 | 0107 << 8 | 0047, // 047
    0313ull << 40 | 0022ull << 32 | 0130u << 24 | 0151 << 16 | 0110 << 8 | 0050, // 050
    0314ull << 40 | 0023ull << 32 | 0106u << 24 | 0152 << 16 | 0111 << 8 | 0051, // 051
    0322ull << 40 | 0031ull << 32 | 0131u << 24 | 0113 << 16 | 0112 << 8 | 0052, // 052
    0316ull << 40 | 0012ull << 32 | 0123u << 24 | 0154 << 16 | 0113 << 8 | 0053, // 053
    0324ull << 40 | 0015ull << 32 | 0102u << 24 | 0115 << 16 | 0114 << 8 | 0054, // 054
    0310ull << 40 | 0013ull << 32 | 0104u << 24 | 0110 << 16 | 0115 << 8 | 0055, // 055
    0326ull << 40 | 0016ull << 32 | 0132u << 24 | 0117 << 16 | 0116 << 8 | 0056, // 056
    0334ull << 40 | 0014ull << 32 | 0105u << 24 | 0160 << 16 | 0117 << 8 | 0057, // 057
    0327ull << 40 | 0000ull << 32 | 0126u << 24 | 0120 << 16 | 0120 << 8 | 0060, // 060
    0303ull << 40 | 0001ull << 32 | 0103u << 24 | 0103 << 16 | 0121 << 8 | 0061, // 061
    0343ull << 40 | 0002ull << 32 | 0120u << 24 | 0124 << 16 | 0122 << 8 | 0062, // 062
    0350ull << 40 | 0003ull << 32 | 0111u << 24 | 0131 << 16 | 0123 << 8 | 0063, // 063
    0276ull << 40 | 0004ull << 32 | 0107u << 24 | 0146 << 16 | 0124 << 8 | 0064, // 064
    0347ull << 40 | 0005ull << 32 | 0122u << 24 | 0130 << 16 | 0125 << 8 | 0065, // 065
    0273ull << 40 | 0006ull << 32 | 0114u << 24 | 0143 << 16 | 0126 << 8 | 0066, // 066
    0376ull << 40 | 0007ull << 32 | 0012u << 24 | 0176 << 16 | 0127 << 8 | 0067, // 067
    0373ull << 40 | 0010ull << 32 | 0115u << 24 | 0173 << 16 | 0130 << 8 | 0070, // 070
    0375ull << 40 | 0011ull << 32 | 0116u << 24 | 0175 << 16 | 0131 << 8 | 0071, // 071
    0357ull << 40 | 0037ull << 32 | 0110u << 24 | 0171 << 16 | 0132 << 8 | 0072, // 072
    0356ull << 40 | 0026ull << 32 | 0040u << 24 | 0170 << 16 | 0173 << 8 | 0073, // 073
    0374ull << 40 | 0035ull << 32 | 0117u << 24 | 0174 << 16 | 0174 << 8 | 0074, // 074
    0270ull << 40 | 0025ull << 32 | 0015u << 24 | 0140 << 16 | 0175 << 8 | 0075, // 075
    0335ull << 40 | 0036ull << 32 | 0124u << 24 | 0161 << 16 | 0176 << 8 | 0076, // 076
    0304ull << 40 | 0021ull << 32 | 0300u << 24 | 0000 << 16 | 0140 << 8 | 0077, // 077
    0306ull << 40 | 0136ull << 32 | 0240u << 24 | 0135 << 16 | 0000 << 8 | 0100, // 100
    0307ull << 40 | 0040ull << 32 | 0113u << 24 | 0054 << 16 | 0000 << 8 | 0101, // 101
    0311ull << 40 | 0042ull << 32 | 0161u << 24 | 0056 << 16 | 0000 << 8 | 0102, // 102
    0321ull << 40 | 0061ull << 32 | 0131u << 24 | 0000 << 16 | 0000 << 8 | 0103, // 103
    0323ull << 40 | 0077ull << 32 | 0340u << 24 | 0100 << 16 | 0000 << 8 | 0104, // 104
    0325ull << 40 | 0045ull << 32 | 0152u << 24 | 0050 << 16 | 0000 << 8 | 0105, // 105
    0330ull << 40 | 0100ull << 32 | 0102u << 24 | 0051 << 16 | 0000 << 8 | 0106, // 106
    0331ull << 40 | 0101ull << 32 | 0101u << 24 | 0000 << 16 | 0000 << 8 | 0107, // 107
    0342ull << 40 | 0055ull << 32 | 0170u << 24 | 0073 << 16 | 0000 << 8 | 0110, // 110
    0344ull << 40 | 0102ull << 32 | 0146u << 24 | 0133 << 16 | 0000 << 8 | 0111, // 111
    0345ull << 40 | 0103ull << 32 | 0171u << 24 | 0040 << 16 | 0000 << 8 | 0112, // 112
    0346ull << 40 | 0052ull << 32 | 0103u << 24 | 0075 << 16 | 0000 << 8 | 0113, // 113
    0351ull << 40 | 0104ull << 32 | 0142u << 24 | 0076 << 16 | 0000 << 8 | 0114, // 114
    0340ull << 40 | 0054ull << 32 | 0144u << 24 | 0000 << 16 | 0000 << 8 | 0115, // 115
    0300ull << 40 | 0105ull << 32 | 0172u << 24 | 0000 << 16 | 0000 << 8 | 0116, // 116
    0320ull << 40 | 0056ull << 32 | 0105u << 24 | 0000 << 16 | 0000 << 8 | 0117, // 117
    0152ull << 40 | 0060ull << 32 | 0166u << 24 | 0000 << 16 | 0000 << 8 | 0120, // 120
    0120ull << 40 | 0106ull << 32 | 0143u << 24 | 0012 << 16 | 0000 << 8 | 0121, // 121
    0133ull << 40 | 0107ull << 32 | 0160u << 24 | 0030 << 16 | 0000 << 8 | 0122, // 122
    0137ull << 40 | 0110ull << 32 | 0151u << 24 | 0032 << 16 | 0000 << 8 | 0123, // 123
    0172ull << 40 | 0062ull << 32 | 0147u << 24 | 0000 << 16 | 0000 << 8 | 0124, // 124
    0176ull << 40 | 0111ull << 32 | 0120u << 24 | 0000 << 16 | 0000 << 8 | 0125, // 125
    0154ull << 40 | 0112ull << 32 | 0154u << 24 | 0000 << 16 | 0000 << 8 | 0126, // 126
    0133ull << 40 | 0113ull << 32 | 0012u << 24 | 0040 << 16 | 0000 << 8 | 0127, // 127
    0152ull << 40 | 0065ull << 32 | 0115u << 24 | 0137 << 16 | 0000 << 8 | 0130, // 130
    0140ull << 40 | 0063ull << 32 | 0110u << 24 | 0077 << 16 | 0000 << 8 | 0131, // 131
    0155ull << 40 | 0114ull << 32 | 0130u << 24 | 0000 << 16 | 0000 << 8 | 0132, // 132
    0117ull << 40 | 0027ull << 32 | 0040u << 24 | 0000 << 16 | 0000 << 8 | 0133, // 133
    0177ull << 40 | 0115ull << 32 | 0117u << 24 | 0000 << 16 | 0000 << 8 | 0134, // 134
    0356ull << 40 | 0030ull << 32 | 0015u << 24 | 0000 << 16 | 0000 << 8 | 0135, // 135
    0174ull << 40 | 0131ull << 32 | 0124u << 24 | 0000 << 16 | 0000 << 8 | 0136, // 136
    0175ull << 40 | 0132ull << 32 | 0300u << 24 | 0000 << 16 | 0000 << 8 | 0137, // 137
    0100ull << 40 | 0075ull << 32 | 0240u << 24 | 0000 << 16 | 0000 << 8 | 0140, // 140
    0100ull << 40 | 0040ull << 32 | 0050u << 24 | 0045 << 16 | 0000 << 8 | 0101, // 141
    0100ull << 40 | 0041ull << 32 | 0061u << 24 | 0127 << 16 | 0000 << 8 | 0142, // 142
    0100ull << 40 | 0066ull << 32 | 0067u << 24 | 0107 << 16 | 0000 << 8 | 0143, // 143
    0100ull << 40 | 0044ull << 32 | 0340u << 24 | 0104 << 16 | 0000 << 8 | 0144, // 144
    0100ull << 40 | 0045ull << 32 | 0140u << 24 | 0072 << 16 | 0000 << 8 | 0105, // 145
    0100ull << 40 | 0064ull << 32 | 0062u << 24 | 0126 << 16 | 0000 << 8 | 0146, // 146
    0100ull << 40 | 0043ull << 32 | 0055u << 24 | 0132 << 16 | 0000 << 8 | 0147, // 147
    0100ull << 40 | 0065ull << 32 | 0057u << 24 | 0111 << 16 | 0000 << 8 | 0130, // 150
    0100ull << 40 | 0050ull << 32 | 0174u << 24 | 0112 << 16 | 0000 << 8 | 0151, // 151
    0100ull << 40 | 0051ull << 32 | 0066u << 24 | 0000 << 16 | 0000 << 8 | 0152, // 152
    0100ull << 40 | 0052ull << 32 | 0047u << 24 | 0114 << 16 | 0000 << 8 | 0113, // 153
    0100ull << 40 | 0053ull << 32 | 0036u << 24 | 0000 << 16 | 0000 << 8 | 0154, // 154
    0100ull << 40 | 0054ull << 32 | 0030u << 24 | 0116 << 16 | 0000 << 8 | 0115, // 155
    0100ull << 40 | 0055ull << 32 | 0053u << 24 | 0000 << 16 | 0000 << 8 | 0110, // 156
    0100ull << 40 | 0056ull << 32 | 0063u << 24 | 0000 << 16 | 0000 << 8 | 0117, // 157
    0100ull << 40 | 0057ull << 32 | 0075u << 24 | 0122 << 16 | 0000 << 8 | 0160, // 160
    0100ull << 40 | 0076ull << 32 | 0072u << 24 | 0123 << 16 | 0000 << 8 | 0161, // 161
    0100ull << 40 | 0060ull << 32 | 0060u << 24 | 0000 << 16 | 0000 << 8 | 0120, // 162
    0100ull << 40 | 0061ull << 32 | 0070u << 24 | 0125 << 16 | 0000 << 8 | 0103, // 163
    0100ull << 40 | 0062ull << 32 | 0173u << 24 | 0106 << 16 | 0000 << 8 | 0124, // 164
    0100ull << 40 | 0063ull << 32 | 0064u << 24 | 0074 << 16 | 0000 << 8 | 0131, // 165
    0100ull << 40 | 0046ull << 32 | 0051u << 24 | 0043 << 16 | 0000 << 8 | 0166, // 166
    0100ull << 40 | 0042ull << 32 | 0012u << 24 | 0000 << 16 | 0000 << 8 | 0102, // 167
    0100ull << 40 | 0073ull << 32 | 0056u << 24 | 0047 << 16 | 0000 << 8 | 0170, // 170
    0100ull << 40 | 0072ull << 32 | 0054u << 24 | 0047 << 16 | 0000 << 8 | 0171, // 171
    0100ull << 40 | 0047ull << 32 | 0175u << 24 | 0041 << 16 | 0000 << 8 | 0172, // 172
    0100ull << 40 | 0070ull << 32 | 0040u << 24 | 0046 << 16 | 0000 << 8 | 0173, // 173
    0100ull << 40 | 0074ull << 32 | 0071u << 24 | 0000 << 16 | 0000 << 8 | 0174, // 174
    0100ull << 40 | 0071ull << 32 | 0015u << 24 | 0044 << 16 | 0000 << 8 | 0175, // 175
    0100ull << 40 | 0067ull << 32 | 0065u << 24 | 0121 << 16 | 0000 << 8 | 0176, // 176
    0100ull << 40 | 0017ull << 32 | 0300u << 24 | 0000 << 16 | 0000 << 8 | 0177, // 177
};
