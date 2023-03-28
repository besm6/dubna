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
#include <fstream>
#include "fixture_machine.h"

TEST_F(dubna_machine, trace_arx)
{
    // Store the test code.
    store_word(010, besm6_asm("xta 2000, arx 2001"));
    store_word(011, besm6_asm("stop 12345(6), utc")); // Magic opcode: Pass
    store_word(02000, 00000000000000013ul);
    store_word(02001, 00000000000000001ul);

    // Enable trace.
    std::string trace_filename = get_test_name() + ".trace";
    machine->redirect_trace(trace_filename.c_str(), "irm");

    // Run the code.
    machine->cpu.set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->cpu.get_pc(), 011u);
    EXPECT_EQ(machine->cpu.get_acc(), 014u);

    // Read the trace.
    auto trace = file_contents_split(trace_filename);

    // Check output.
    static const std::vector<std::string> expect = {
        "00010 L: 00 010 2000 xta 2000",
        "      Memory Read [02000] = 0000 0000 0000 0013",
        "      Write ACC = 0000 0000 0000 0013",
        "      Write RAU = 04",
        "00010 R: 00 013 2001 arx 2001",
        "      Memory Read [02001] = 0000 0000 0000 0001",
        "      Write ACC = 0000 0000 0000 0014",
        "      Write RAU = 10",
        "00011 L: 06 33 12345 stop 12345(6)",
    };
    EXPECT_EQ(trace, expect);
}

TEST_F(dubna_machine, e70_read_drum)
{
    // Write test data to memory.
    static const Words input = {
        0'7760'0000'0000'0067,
        0'1302'7055'1203'1060,
        0'0000'2342'5442'0001,
        0'2224'2502'1422'7060,
        0'0000'1261'5401'2011,
        0'1442'0107'0360'7417,
        0'0000'1261'5401'2016,
        0'1242'0055'1002'5417,
        0'0000'1261'5401'2023,
        0'1363'0056'1160'7417,
    };
    machine->memory.write_words(input, 02000);

    // Write page to drum.
    machine->drum_io('w', 020, 7, 0, 02000, 1024);

    // Store the test code.
    store_word(010, besm6_asm("*70 2000, utc"));
    store_word(011, besm6_asm("stop 12345(6), utc")); // Magic opcode: Pass
    store_word(02000, 00010220000200007ul); // Read drum 20 zone 7 into page 22, address 044000

    // Enable trace.
    std::string trace_filename = get_test_name() + ".trace";
    machine->redirect_trace(trace_filename.c_str(), "irm");

    // Run the code.
    machine->cpu.set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->cpu.get_pc(), 011u);

    // Check data in memory.
    Words result;
    machine->memory.read_words(result, input.size(), 044000);
    EXPECT_EQ(result, input);
}

TEST_F(dubna_machine, trace_startjob)
{
    // Phys i/o: map drum 021 to disk 030.
    machine->map_drum_to_disk(021, 030);

    store_word(02010, besm6_asm("vtm -5(1),     *70 3002"));     // читаем инициатор монитора
    store_word(02011, besm6_asm("xta 377,       atx 3010"));     // берем тракт, где MONITOR* + /MONTRAN
    store_word(02012, besm6_asm("xta 363,       atx 100"));      // ТРП для загрузчика
    store_word(02013, besm6_asm("vtm 53401(17), utc"));          // магазин
    store_word(02014, besm6_asm("*70 3010(1),   utc"));          // каталоги
    store_word(02015, besm6_asm("vlm 2014(1),   ita 17"));       // aload по адресу 716b
    store_word(02016, besm6_asm("atx 716,       *70 717"));      // infloa по адресу 717b - статический загрузчик
    store_word(02017, besm6_asm("xta 17,        ati 16"));       //
    store_word(02020, besm6_asm("atx 2(16),     arx 3001"));     // прибавляем 10b
    store_word(02021, besm6_asm("atx 17,        xta 3000"));     // 'INPUTCAL'
    store_word(02022, besm6_asm("atx (16),      vtm 1673(15)")); // call CHEKJOB*
    store_word(02023, besm6_asm("uj (17),       utc"));          // в статический загрузчик

    store_word(03000, 05156606564434154ul); // 'INPUTCAL' in Text encoding
    store_word(03001, 00000000000000010ul); // прибавляем 10b
    store_word(03002, 04014000000210201ul); // инициатор
    store_word(03003, 00000000000200000ul); //  Т Р П
    store_word(03004, 00014000000210007ul); // каталоги
    store_word(03005, 00000000000210000ul); // временной
    store_word(03006, 00014000000210010ul); // библиотеки
    store_word(03007, 00000000000210001ul); // (физ. и мат.)
    store_word(03010, 00014000000210035ul); // /MONTRAN

    // Enable trace.
    std::string trace_filename = get_test_name() + ".trace";
    machine->redirect_trace(trace_filename.c_str(), "eirm");

    // Run the code.
    machine->cpu.set_pc(02010);
    machine->run();

    // Read the trace.
    auto trace = file_contents_split(trace_filename);

    // Check output.
    ASSERT_EQ(trace[0], "02010 L: 01 24 77773 vtm -5(1)");
    ASSERT_EQ(trace[1], "      Write M1 = 77773");
    ASSERT_EQ(trace[2], "02010 R: 00 070 3002 *70 3002");
    ASSERT_EQ(trace[3], "TODO");
}

#if 0
TEST_F(dubna_machine, end_file)
{
    load_and_run(R"(
        *end file
    )");

    // Check status.
    EXPECT_EQ(Machine::get_instr_count(), 2);
}
#endif
