//
// Tests for COSY format.
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
#include "fixture_machine.h"

TEST(unit, encode_cosy)
{
    EXPECT_EQ(encode_cosy(""),         "\323\n   \n");
    EXPECT_EQ(encode_cosy("A"),        "A\322\n  \n");
    EXPECT_EQ(encode_cosy("AB"),       "AB\321\n \n");
    EXPECT_EQ(encode_cosy("ABC"),      "ABC\320\n\n");
    EXPECT_EQ(encode_cosy("ABCD"),     "ABCD\317\n");
    EXPECT_EQ(encode_cosy("ABCDE"),    "ABCDE\316\n    \n");
    EXPECT_EQ(encode_cosy("ABCDEF"),   "ABCDEF\315\n   \n");
    EXPECT_EQ(encode_cosy("ABC DEF"),  "ABC\201DEF\314\n  \n");
    EXPECT_EQ(encode_cosy("ABC  DEF"), "ABC\202DEF\313\n  \n");
}

TEST_F(dubna_machine, cosy_2lines)
{
    // Write input.
    unsigned offset = 0;
    machine->drum_write_cosy(1, offset, "*name empty");
    machine->drum_write_cosy(1, offset, "*end file");

    // Check result on drum.
    unsigned const drum1 = 01;
    EXPECT_EQ(machine->drum_read_word(drum1, 0), 0'1244'7101'2324'2601);
    EXPECT_EQ(machine->drum_read_word(drum1, 1), 0'2124'6520'2505'4710);
    EXPECT_EQ(machine->drum_read_word(drum1, 2), 0'0242'0040'1002'0012);
    EXPECT_EQ(machine->drum_read_word(drum1, 3), 0'1244'2516'2110'0506);
    EXPECT_EQ(machine->drum_read_word(drum1, 4), 0'2224'6105'6240'5012);
}

TEST_F(dubna_machine, cosy_4lines)
{
    // Write input.
    unsigned offset = 0;
    machine->drum_write_cosy(1, offset, "");
    machine->drum_write_cosy(1, offset, "a");
    machine->drum_write_cosy(1, offset, "ab");
    machine->drum_write_cosy(1, offset, "abc");

    // Check result on drum.
    unsigned const drum1 = 01;
    EXPECT_EQ(machine->drum_read_word(drum1, 0), 0'6460'5040'1002'0012);
    EXPECT_EQ(machine->drum_read_word(drum1, 1), 0'2035'1012'1002'0012);
    EXPECT_EQ(machine->drum_read_word(drum1, 2), 0'2024'1321'0242'0012);
    EXPECT_EQ(machine->drum_read_word(drum1, 3), 0'2024'1103'6400'5012);
}
