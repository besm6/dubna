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
    std::string trace_filename = get_test_name() + ".trace";

    // Store the test code.
    store_word(010, besm6_asm("сч 2000, слц 2001"));
    store_word(011, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(02000, 00000000000000013ul);
    store_word(02001, 00000000000000001ul);

    // Run the code.
    machine->cpu.set_pc(010);
    machine->redirect_trace(trace_filename.c_str(), "irm");
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->cpu.get_pc(), 011u);
    EXPECT_EQ(machine->cpu.get_acc(), 014u);

    // Read the trace.
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
    auto trace = file_contents_split(trace_filename);

    // Check output.
    ASSERT_EQ(trace.size(), expect.size());
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
