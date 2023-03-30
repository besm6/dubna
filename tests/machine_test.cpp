//
// Tests for Machine class.
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
    store_word(02000, 0'0000'0000'0000'0013ul);
    store_word(02001, 0'0000'0000'0000'0001ul);

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
    store_word(02000, 0'0010'2200'0020'0007ul); // Read drum 20 zone 7 into page 22, address 044000

    // Enable trace for debug.
    std::string trace_filename = get_test_name() + ".trace";
    machine->redirect_trace(trace_filename.c_str(), "eirm");

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
    // Open disk monsys.9, for read only.
    machine->disk_mount(030, TEST_DIR "/../tapes/9", false);

    // Phys i/o: map drum 021 to disk 030.
    machine->map_drum_to_disk(021, 030);

    // Load boot code for Monitoring System Dubna.
    machine->boot_ms_dubna();

    // Write job to the input drum.
    static const Words input = {
        0'1244'7101'2324'2601,
        0'2124'6520'2505'4710,
        0'0242'0040'1002'0012,
        0'1244'2516'2110'0506,
        0'2224'6105'6240'5012,
        0'1245'1105'2024'2040,
        0'2364'6104'6240'5012,
        0'1244'2516'2102'0106,
        0'2224'6105'1014'4412,
    };
    machine->memory.write_words(input, 04000);
    machine->drum_io('w', 001, 0, 0, 04000, 1024);

    // Trace extracodes.
    std::string trace_filename = get_test_name() + ".trace";
    machine->redirect_trace(trace_filename.c_str(), "eirm");

    // Run the code.
    machine->run();

    // Check PC value.
    ASSERT_EQ(machine->cpu.get_pc(), 1554);

    // Read the trace.
    auto trace = file_contents(trace_filename);

    // Check output.
    static const std::string expect = R"(02010 R: 00 070 3002 *70 3002
      Drum 21 PhysRead [00000-00377] = Zone 1 Sector 2
02014 L: 01 070 3010 *70 3010(1)
      Drum 20 Write [00000-01777] = Zone 0
02014 L: 01 070 3010 *70 3010(1)
      Drum 21 PhysRead [00000-01777] = Zone 7
02014 L: 01 070 3010 *70 3010(1)
      Drum 21 Write [00000-01777] = Zone 0
02014 L: 01 070 3010 *70 3010(1)
      Drum 21 PhysRead [00000-01777] = Zone 10
02014 L: 01 070 3010 *70 3010(1)
      Drum 21 Write [00000-01777] = Zone 1
02014 L: 01 070 3010 *70 3010(1)
      Drum 21 PhysRead [00000-01777] = Zone 35
02016 R: 00 070 0717 *70 717
      Drum 21 PhysRead [53400-53777] = Zone 1 Sector 1
53442 R: 01 070 0373 *70 373(1)
      Drum 21 PhysRead [76000-76377] = Zone 1 Sector 3
53475 R: 01 070 0373 *70 373(1)
      Drum 21 PhysRead [76000-77777] = Zone 36
53554 R: 01 070 0373 *70 373(1)
      Drum 21 PhysRead [76000-77777] = Zone 37
53554 R: 01 070 0373 *70 373(1)
      Drum 22 PhysRead [76000-77777] = Zone 0
53554 R: 01 070 0373 *70 373(1)
      Drum 22 PhysRead [76000-77777] = Zone 1
53554 R: 01 070 0373 *70 373(1)
      Drum 22 PhysRead [76000-77777] = Zone 2
53554 R: 01 070 0373 *70 373(1)
      Drum 22 PhysRead [76000-77777] = Zone 3
53613 L: 00 070 0045 *70 45
      Drum 20 Read [76000-77777] = Zone 4
01525 R: 00 050 0067 *50 67
01525 R: 00 050 0067 *50 67
01547 R: 00 064 0000 *64
02551 R: 00 070 0073 *70 73
      Drum 21 PhysRead [76000-77777] = Zone 1
02561 L: 00 063 0765 *63 765
02563 L: 00 063 0007 *63 7
02567 L: 00 063 0502 *63 502
02570 R: 16 063 0000 *63 (16)
01414 L: 07 064 0077 *64 77(7)
01414 L: 07 064 0077 *64 77(7)
02602 L: 00 064 3036 *64 3036
01160 L: 13 070 0062 *70 62(13)
      Drum 1 Read [76000-77777] = Zone 0
01414 L: 07 064 0077 *64 77(7)
03021 L: 00 074 0000 *74
)";
    EXPECT_EQ(trace, expect);
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
