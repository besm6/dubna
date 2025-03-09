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
#include <filesystem>

#include "fixture_machine.h"
#include "cosy.h"

#include "encoding.h"

TEST(unit, encode_cosy)
{
    EXPECT_EQ(encode_cosy(""), "\323\n   \n");
    EXPECT_EQ(encode_cosy("A"), "A\322\n  \n");
    EXPECT_EQ(encode_cosy("AB"), "AB\321\n \n");
    EXPECT_EQ(encode_cosy("ABC"), "ABC\320\n\n");
    EXPECT_EQ(encode_cosy("ABCD"), "ABCD\317\n");
    EXPECT_EQ(encode_cosy("ABCDE"), "ABCDE\316\n    \n");
    EXPECT_EQ(encode_cosy("ABCDEF"), "ABCDEF\315\n   \n");
    EXPECT_EQ(encode_cosy("ABC DEF"), "ABC\201DEF\314\n  \n");
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
    EXPECT_EQ(machine->drum_read_word(drum1, 0), 0'1244'7101'2324'2601u);
    EXPECT_EQ(machine->drum_read_word(drum1, 1), 0'2124'6520'2505'4710u);
    EXPECT_EQ(machine->drum_read_word(drum1, 2), 0'0242'0040'1002'0012u);
    EXPECT_EQ(machine->drum_read_word(drum1, 3), 0'1244'2516'2110'0506u);
    EXPECT_EQ(machine->drum_read_word(drum1, 4), 0'2224'6105'6240'5012u);
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
    EXPECT_EQ(machine->drum_read_word(drum1, 0), 0'6460'5040'1002'0012u);
    EXPECT_EQ(machine->drum_read_word(drum1, 1), 0'2035'1012'1002'0012u);
    EXPECT_EQ(machine->drum_read_word(drum1, 2), 0'2024'1321'0242'0012u);
    EXPECT_EQ(machine->drum_read_word(drum1, 3), 0'2024'1103'6400'5012u);
}

TEST(unit, file_txt_to_cosy)
{
    const std::string path_txt = get_test_name() + ".txt";
    const std::string path_bin = get_test_name() + ".bin";
    const std::string contents = R"(
the quick brown fox jumps over the lazy dog
съешь же ещё этих мягких французских булок, да выпей чаю
)";
    create_file(path_txt, contents);
    std::filesystem::remove(path_bin);
    EXPECT_TRUE(file_txt_to_cosy(path_bin));

    // Check resulting file.
    auto const result = file_contents(path_bin);
    auto expect = file_contents(TEST_DIR "/expect_cosy.bin");
    EXPECT_EQ(result, expect);
}

TEST(unit, good_cosy_to_txt)
{
    const std::string path_txt = get_test_name() + ".txt";
    const std::string path_bin = get_test_name() + ".bin";
    const std::string good_cosy = "ABRA\201CADABRA\307\n   \n"
                                  "KREKS\201FEKS\201PEKS\304\n\n"
                                  "*READ OLD\312\n\n"
                                  "*END FILE \311\n";
    create_file(path_bin, good_cosy);
    std::filesystem::remove(path_txt);
    EXPECT_TRUE(file_cosy_to_txt(path_bin));

    // Check resulting file.
    auto const result = file_contents(path_txt);
    const std::string expect = "ABRA CADABRA\n"
                               "KREKS FEKS PEKS\n";
    EXPECT_EQ(result, expect);
}

TEST(unit, bad_cosy_to_txt)
{
    const std::string path_txt = get_test_name() + ".txt";
    const std::string path_bin = get_test_name() + ".bin";
    const std::string bad_cosy = "ABRA\201CADABRA\307\n   \n"
                                 "KREKS\201FEKS\201PEKS\304\n\n"
                                 "*READ\201OLD\312\n\n" // note packed space - wrong
                                 "*END FILE \311\n";
    create_file(path_bin, bad_cosy);
    std::filesystem::remove(path_txt);
    EXPECT_FALSE(file_cosy_to_txt(path_bin));
    EXPECT_FALSE(std::filesystem::exists(path_txt));
}

TEST(unit, utf8_to_koi7)
{
    EXPECT_EQ(utf8_to_koi7("!\"#$%&'()*+,-./", 16), "!\"#$%&'()*+,-./");
    EXPECT_EQ(utf8_to_koi7("0123456789:;<=>?", 16), "0123456789:;<=>?");
    EXPECT_EQ(utf8_to_koi7("@ABCDEFGHIJKLMNO", 16), "@ABCDEFGHIJKLMNO");
    EXPECT_EQ(utf8_to_koi7("PQRSTUVWXYZ[\\]^_", 16), "PQRSTUVWXYZ[\\]^_");
    EXPECT_EQ(utf8_to_koi7("ЮAБЦДEФГXИЙKЛMHO", 16), "`AbcdEfgXijKlMHO");
    EXPECT_EQ(utf8_to_koi7("ПЯPCTYЖBЬЫЗШЭЩЧ ", 16), "pqPCTYvBxyz{|}~ ");

    EXPECT_EQ(utf8_to_koi7("Ъ", 1), "\5");
    //TODO: EXPECT_EQ(utf8_to_koi7("×", 1), "\6");
    //TODO: EXPECT_EQ(utf8_to_koi7("≤", 1), "\16");
    //TODO: EXPECT_EQ(utf8_to_koi7("≥", 1), "\17");
    //TODO: EXPECT_EQ(utf8_to_koi7("‘", 1), "\20");
    //TODO: EXPECT_EQ(utf8_to_koi7("―", 1), "\25");
    //TODO: EXPECT_EQ(utf8_to_koi7("↑", 1), "\26");
    //TODO: EXPECT_EQ(utf8_to_koi7("⏨", 1), "\27");

    //TODO: EXPECT_EQ(utf8_to_koi7("≠", 1), "\30");
    //TODO: EXPECT_EQ(utf8_to_koi7("°", 1), "\31");
    //TODO: EXPECT_EQ(utf8_to_koi7("÷", 1), "\32");
    //TODO: EXPECT_EQ(utf8_to_koi7("’", 1), "\33");
    //TODO: EXPECT_EQ(utf8_to_koi7("⊃", 1), "\34");
    //TODO: EXPECT_EQ(utf8_to_koi7("≡", 1), "\35");
    //TODO: EXPECT_EQ(utf8_to_koi7("∨", 1), "\36");
    //TODO: EXPECT_EQ(utf8_to_koi7("¬", 1), "\37");

    EXPECT_EQ(utf8_to_koi7("≠", 1), "#");
    //TODO: EXPECT_EQ(utf8_to_koi7("◇", 1), "$");
    EXPECT_EQ(utf8_to_koi7("∧", 1), "^");
    EXPECT_EQ(utf8_to_koi7("′", 1), "'");
    EXPECT_EQ(utf8_to_koi7("↑", 1), "@");
    //TODO: EXPECT_EQ(utf8_to_koi7("‾", 1), "^");
    //TODO: EXPECT_EQ(utf8_to_koi7("|", 1), "|");
}
