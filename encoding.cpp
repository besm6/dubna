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
    /* 020-027 */ 0x65,   0x2191, 0x28,   0x29,   0xd7,   0x3d,   0x3b,   0x5b,
    /* 030-037 */ 0x5d,   0x2a,   0x2018, 0x2019, 0x2260, 0x3c,   0x3e,   0x3a,
    /* 040-047 */ 0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417,
    /* 050-057 */ 0x0418, 0x0419, 0x041a, 0x041b, 0x041c, 0x041d, 0x041e, 0x041f,
    /* 060-067 */ 0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427,
    /* 070-077 */ 0x0428, 0x0429, 0x042b, 0x042c, 0x042d, 0x042e, 0x042f, 0x44,
    /* 100-107 */ 0x46,   0x47,   0x49,   0x4a,   0x4c,   0x4e,   0x51,   0x52,
    /* 110-117 */ 0x53,   0x55,   0x56,   0x57,   0x5a,   0x203e, 0x2264, 0x2265,
    /* 120-127 */ 0x2228, 0x2227, 0x2283, 0xac,   0xf7,   0x2261, 0x25,   0x25c7,
    /* 130-137 */ 0x7c,   0x2015, 0x5f,   0x21,   0x22,   0x042a, 0xb0,   0x2032,
};

static const unsigned short gost_to_unicode_lat[256] = {
    /* 000-007 */ 0x30,   0x31,   0x32,   0x33,   0x34,   0x35,   0x36,   0x37,
    /* 010-017 */ 0x38,   0x39,   0x2b,   0x2d,   0x2f,   0x2c,   0x2e,   0x20,
    /* 020-027 */ 0x65,   0x2191, 0x28,   0x29,   0xd7,   0x3d,   0x3b,   0x5b,
    /* 030-037 */ 0x5d,   0x2a,   0x2018, 0x2019, 0x2260, 0x3c,   0x3e,   0x3a,
    /* 040-047 */ 0x41,   0x0411, 0x42,   0x0413, 0x0414, 0x45,   0x0416, 0x0417,
    /* 050-057 */ 0x0418, 0x0419, 0x4b,   0x041b, 0x4d,   0x48,   0x4f,   0x041f,
    /* 060-067 */ 0x50,   0x43,   0x54,   0x59,   0x0424, 0x58,   0x0426, 0x0427,
    /* 070-077 */ 0x0428, 0x0429, 0x042b, 0x042c, 0x042d, 0x042e, 0x042f, 0x44,
    /* 100-107 */ 0x46,   0x47,   0x49,   0x4a,   0x4c,   0x4e,   0x51,   0x52,
    /* 110-117 */ 0x53,   0x55,   0x56,   0x57,   0x5a,   0x203e, 0x2264, 0x2265,
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
// Convert to UTF-8 encoding:
// 00000000.0xxxxxxx -> 0xxxxxxx
// 00000xxx.xxyyyyyy -> 110xxxxx, 10yyyyyy
// xxxxyyyy.yyzzzzzz -> 1110xxxx, 10yyyyyy, 10zzzzzz
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
    if (ch < 0x80) {
        std::cout << (char)ch;
        return;
    }
    if (ch < 0x800) {
        std::cout << (char)(ch >> 6 | 0xc0);
        std::cout << (char)((ch & 0x3f) | 0x80);
        return;
    }
    std::cout << (char)(ch >> 12 | 0xe0);
    std::cout << (char)(((ch >> 6) & 0x3f) | 0x80);
    std::cout << (char)((ch & 0x3f) | 0x80);
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
        /* XYZ[\]^_ */ 0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
        /* `abcdefg */ 0,    0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, //  ABCDEFG
        /* hijklmno */ 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, // HIJKLMNO
        /* pqrstuvw */ 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, // PQRSTUVW
        /* xyz{|}~  */ 0x58, 0x59, 0x5a, 0,    0,    0,    0,    0,    // XYZ
        /* 80 - 87 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* 88 - 8f */  0,    0,    0,    0,    0,    0,    0,    0,
        /* 90 - 97 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* 98 - 9f */  0,    0,    0,    0,    0,    0,    0,    0,
        /* a0 - a7 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* a8 - af */  0,    0,    0,    0,    0,    0,    0,    0,
        /* b0 - b7 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* b8 - bf */  0,    0,    0,    0,    0,    0,    0,    0,
        /* c0 - c7 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* c8 - cf */  0,    0,    0,    0,    0,    0,    0,    0,
        /* d0 - d7 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* d8 - df */  0,    0,    0,    0,    0,    0,    0,    0,
        /* e0 - e7 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* e8 - ef */  0,    0,    0,    0,    0,    0,    0,    0,
        /* f0 - f7 */  0,    0,    0,    0,    0,    0,    0,    0,
        /* f8 - ff */  0,    0,    0,    0,    0,    0,    0,    0,
        // clang-format on
    };
    switch (val >> 8) {
    case 0x00:
        return tab0[val];
    case 0x04:
        switch ((unsigned char)val) {
        case 0x01:
            return 'E';  // Ë - 0x65
        case 0x10:
            return 'A';  // А - 0x61
        case 0x11:
            return 0x62; // Б
        case 0x12:
            return 'B';  // В - 0x77
        case 0x13:
            return 0x67; // Г
        case 0x14:
            return 0x64; // Д
        case 0x15:
            return 'E';  // Е - 0x65
        case 0x16:
            return 0x76; // Ж
        case 0x17:
            return 0x7a; // З
        case 0x18:
            return 0x69; // И
        case 0x19:
            return 0x6a; // Й
        case 0x1a:
            return 'K';  // К - 0x6b
        case 0x1b:
            return 0x6c; // Л
        case 0x1c:
            return 'M';  // М - 0x6d
        case 0x1d:
            return 'H';  // Н - 0x6e
        case 0x1e:
            return 'O';  // О - 0x6f
        case 0x1f:
            return 0x70; // П
        case 0x20:
            return 'P';  // Р - 0x72
        case 0x21:
            return 'C';  // С - 0x73
        case 0x22:
            return 'T';  // Т - 0x74
        case 0x23:
            return 'Y';  // У - 0x75
        case 0x24:
            return 0x66; // Ф
        case 0x25:
            return 'X';  // Х - 0x68
        case 0x26:
            return 0x63; // Ц
        case 0x27:
            return 0x7e; // Ч
        case 0x28:
            return 0x7b; // Ш
        case 0x29:
            return 0x7d; // Щ
        case 0x2a:
            return 0x78; // Ъ
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
            return 'A';  // а - 0x61
        case 0x31:
            return 0x62; // б
        case 0x32:
            return 'B';  // в - 0x77
        case 0x33:
            return 0x67; // г
        case 0x34:
            return 0x64; // д
        case 0x35:
            return 'E';  // е - 0x65
        case 0x36:
            return 0x76; // ж
        case 0x37:
            return 0x7a; // з
        case 0x38:
            return 0x69; // и
        case 0x39:
            return 0x6a; // й
        case 0x3a:
            return 'K';  // к - 0x6b
        case 0x3b:
            return 0x6c; // л
        case 0x3c:
            return 'M';  // м - 0x6d
        case 0x3d:
            return 'H';  // н - 0x6e
        case 0x3e:
            return 'O';  // о - 0x6f
        case 0x3f:
            return 0x70; // п
        case 0x40:
            return 'P';  // р - 0x72
        case 0x41:
            return 'C';  // с - 0x73
        case 0x42:
            return 'T';  // т - 0x74
        case 0x43:
            return 'Y';  // у - 0x75
        case 0x44:
            return 0x66; // ф
        case 0x45:
            return 'X';  // х - 0x68
        case 0x46:
            return 0x63; // ц
        case 0x47:
            return 0x7e; // ч
        case 0x48:
            return 0x7b; // ш
        case 0x49:
            return 0x7d; // щ
        case 0x4a:
            return 0x78; // ъ
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
            return 'E';  // ё - 0x65
        }
        break;
    case 0x20:
        switch ((unsigned char)val) {
        case 0x15:
            return '-';
        case 0x18:
            return 0; // `
        case 0x19:
            return '\'';
        case 0x28:
            return 0x0a;
        case 0x32:
            return '\'';
        }
        break;
    case 0x21:
        switch ((unsigned char)val) {
        case 0x2f:
            return 'E';
        case 0x91:
            return '@';
        }
        break;
    case 0x22:
        switch ((unsigned char)val) {
        case 0x27:
            return '^';
        case 0x28:
            return 'v';
        case 0x60:
            return '#';
        }
        break;
    case 0x25:
        switch ((unsigned char)val) {
        case 0xca:
            return '$';
        }
        break;
    }
    return 0;
}

//
// Convert string from UTF-8 encoding to KOI-7.
//
std::string utf8_to_koi7(const std::string &input)
{
    const char *ptr = input.c_str();
    std::string line;
    while (line.size() < 80) {
        // Get unicode character.
        unsigned u = utf8_to_unicode(&ptr);
        if (!u)
            break;

        // Convert to KOI-7.
        unsigned ch = unicode_to_koi7(u);
        if (ch < ' ')
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
