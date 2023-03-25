//
// Unit tests for BESM-6 CPU instructions.
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

//
// Test: UJ instruction (ПБ).
//
TEST_F(dubna_machine, uj)
{
    // Store the test code.
    //
    //  start   старт   '10'
    //          пб      pass
    //  fail    стоп    '76543'(2)
    //  pass    стоп    '12345'(6)
    //
    store_word(010, besm6_asm("00 30 00012, 00 22 00000"));
    store_word(011, besm6_asm("02 33 76543, 00 22 00000"));
    store_word(012, besm6_asm("06 33 12345, 00 22 00000"));

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check the PC address.
    EXPECT_EQ(machine->get_pc(), 012u);
}

//
// Test: VTM, VZM, V1M instructions (УИА, ПИО, ПИНО).
//
TEST_F(dubna_machine, vtm_vzm_v1m)
{
    // Store the test code.
    //
    //  start   старт   '10'
    //          уиа     0(2)
    //          пио     ok(2)
    //          пб      fail
    //  ok      пино    fail(2)
    //          пино    fail(2)
    //          уиа     -1(2)
    //          пио     fail(2)
    //          пио     fail(2)
    //          пино    pass(2)
    //  fail    стоп    '76543'(2)
    //  pass    стоп    '12345'(6)
    //
    store_word(010, besm6_asm("уиа (2), пио 12(2)"));
    store_word(011, besm6_asm("пб 15, мода"));
    store_word(012, besm6_asm("пино 15(2), пино 15(2)"));
    store_word(013, besm6_asm("уиа -1(2), пио 15(2)"));
    store_word(014, besm6_asm("пио 15(2), пино 16(2)"));
    store_word(015, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(016, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 016u);
    EXPECT_EQ(machine->get_m(2), 077777u);
}

//
// Test: J+M, UTM instructions (СЛИ, СЛИА).
//
TEST_F(dubna_machine, jam_utm)
{
    // Store the test code.
    store_word(010, besm6_asm("уиа 1(2), уиа -17(3)"));
    store_word(011, besm6_asm("пб 13, мода"));
    store_word(012, besm6_asm("сли 2(2), слиа 1(3)"));
    store_word(013, besm6_asm("пино 12(2), пино 15(3)"));
    store_word(014, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(015, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 014u);
    EXPECT_EQ(machine->get_m(2), 0u);
    EXPECT_EQ(machine->get_m(3), 0u);
}

//
// Test: VLM instruction (ЦИКЛ).
//
TEST_F(dubna_machine, vlm)
{
    // Store the test code.
    store_word(0010, besm6_asm("уиа -11(2), уиа -12(3)"));
    store_word(0011, besm6_asm("слиа 1(3), цикл 11(2)"));
    store_word(0012, besm6_asm("пино 105(2), пино 105(3)"));
    store_word(0013, besm6_asm("цикл 105(2), пино 105(2)"));
    store_word(0014, besm6_asm("сч 2000, уиа 77401(16)"));
    store_word(0015, besm6_asm("зп 2400(16), цикл 15(16)"));
    store_word(0016, besm6_asm("сч, уиа 77401(17)"));
    store_word(0017, besm6_asm("слц 2400(17), цикл 17(17)"));
    store_word(0020, besm6_asm("нтж, по 105"));
    store_word(0021, besm6_asm("нтж 2000, пе 105"));
    store_word(0022, besm6_asm("уиа 77401(16), уиа 77401(15)"));
    store_word(0023, besm6_asm("слц 2400(16), цикл 23(16)"));
    store_word(0024, besm6_asm("нтж, по 105"));
    store_word(0025, besm6_asm("нтж 2000, пе 105"));
    store_word(0026, besm6_asm("слц 2400(15), цикл 26(15)"));
    store_word(0027, besm6_asm("нтж, по 105"));
    store_word(0030, besm6_asm("нтж 2000, пе 105"));
    store_word(0031, besm6_asm("уиа 77401(14), уиа 77401(13)"));
    store_word(0032, besm6_asm("слц 2400(14), цикл 32(14)"));
    store_word(0033, besm6_asm("нтж, по 105"));
    store_word(0034, besm6_asm("нтж 2000, пе 105"));
    store_word(0035, besm6_asm("слц 2400(13), цикл 35(13)"));
    store_word(0036, besm6_asm("нтж, по 105"));
    store_word(0037, besm6_asm("нтж 2000, пе 105"));
    store_word(0040, besm6_asm("уиа 77401(12), уиа 77401(11)"));
    store_word(0041, besm6_asm("слц 2400(12), цикл 41(12)"));
    store_word(0042, besm6_asm("нтж, по 105"));
    store_word(0043, besm6_asm("нтж 2000, пе 105"));
    store_word(0044, besm6_asm("слц 2400(11), цикл 44(11)"));
    store_word(0045, besm6_asm("нтж, по 105"));
    store_word(0046, besm6_asm("нтж 2000, пе 105"));
    store_word(0047, besm6_asm("уиа 77401(10), уиа 77401(7)"));
    store_word(0050, besm6_asm("слц 2400(10), цикл 50(10)"));
    store_word(0051, besm6_asm("нтж, по 105"));
    store_word(0052, besm6_asm("нтж 2000, пе 105"));
    store_word(0053, besm6_asm("слц 2400(7), цикл 53(7)"));
    store_word(0054, besm6_asm("нтж, по 105"));
    store_word(0055, besm6_asm("нтж 2000, пе 105"));
    store_word(0056, besm6_asm("уиа 77401(6), уиа 77401(5)"));
    store_word(0057, besm6_asm("слц 2400(6), цикл 57(6)"));
    store_word(0060, besm6_asm("нтж, по 105"));
    store_word(0061, besm6_asm("нтж 2000, пе 105"));
    store_word(0062, besm6_asm("слц 2400(5), цикл 62(5)"));
    store_word(0063, besm6_asm("нтж, по 105"));
    store_word(0064, besm6_asm("нтж 2000, пе 105"));
    store_word(0065, besm6_asm("уиа 77401(4), уиа 77401(3)"));
    store_word(0066, besm6_asm("слц 2400(4), цикл 66(4)"));
    store_word(0067, besm6_asm("нтж, по 105"));
    store_word(0070, besm6_asm("нтж 2000, пе 105"));
    store_word(0071, besm6_asm("слц 2400(3), цикл 71(3)"));
    store_word(0072, besm6_asm("нтж, по 105"));
    store_word(0073, besm6_asm("нтж 2000, пе 105"));
    store_word(0074, besm6_asm("уиа 77401(2), мода"));
    store_word(0075, besm6_asm("слц 2400(2), цикл 75(2)"));
    store_word(0076, besm6_asm("нтж, по 105"));
    store_word(0077, besm6_asm("нтж 2000, пе 105"));
    store_word(0100, besm6_asm("уиа 77401(1), мода"));
    store_word(0101, besm6_asm("слц 2400(1), цикл 101(1)"));
    store_word(0102, besm6_asm("нтж, по 105"));
    store_word(0103, besm6_asm("нтж 2000, пе 105"));
    store_word(0104, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(0105, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02000, 07777777777777777ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 0104u);
    EXPECT_EQ(machine->get_m(1), 0u);
    EXPECT_EQ(machine->get_m(2), 0u);
    EXPECT_EQ(machine->get_m(3), 0u);
    EXPECT_EQ(machine->get_m(4), 0u);
    EXPECT_EQ(machine->get_m(5), 0u);
    EXPECT_EQ(machine->get_m(6), 0u);
    EXPECT_EQ(machine->get_m(7), 0u);
    EXPECT_EQ(machine->get_m(8), 0u);
    EXPECT_EQ(machine->get_m(9), 0u);
    EXPECT_EQ(machine->get_m(10), 0u);
    EXPECT_EQ(machine->get_m(11), 0u);
    EXPECT_EQ(machine->get_m(12), 0u);
    EXPECT_EQ(machine->get_m(13), 0u);
    EXPECT_EQ(machine->get_m(14), 0u);
    EXPECT_EQ(machine->get_m(15), 0u);
}

//
// Test: UTC, WTC instructions (МОДА, МОД).
//
TEST_F(dubna_machine, utc_wtc)
{
    // Store the test code.
    store_word(010, besm6_asm("мода -1, уиа (3)"));
    store_word(011, besm6_asm("пио 40(3), слиа 1(3)"));
    store_word(012, besm6_asm("пино 40(3), мода"));
    store_word(013, besm6_asm("мода -1, мода"));
    store_word(014, besm6_asm("уиа (3), пио 40(3)"));
    store_word(015, besm6_asm("слиа 1(3), пино 40(3)"));
    store_word(016, besm6_asm("мод 2000, уиа (3)"));
    store_word(017, besm6_asm("пио 40(3), слиа 1(3)"));
    store_word(020, besm6_asm("пино 40(3), мод 2000"));
    store_word(021, besm6_asm("уиа (3), пио 40(3)"));
    store_word(022, besm6_asm("слиа 1(3), пино 40(3)"));
    store_word(023, besm6_asm("мода -7, мода 10"));
    store_word(024, besm6_asm("уиа -2(3), пио 40(3)"));
    store_word(025, besm6_asm("слиа 1(3), пино 40(3)"));
    store_word(026, besm6_asm("мод 2000, мода 10"));
    store_word(027, besm6_asm("уиа -6(3), слиа -1(3)"));
    store_word(030, besm6_asm("пино 40(3), уиа -1(3)"));
    store_word(031, besm6_asm("мод 2002(3), уиа (4)"));
    store_word(032, besm6_asm("уии 5(4), слиа 52526(5)"));
    store_word(033, besm6_asm("пино 40(5), слиа 1(3)"));
    store_word(034, besm6_asm("мод 2002(3), уиа (4)"));
    store_word(035, besm6_asm("уии 5(4), слиа 25253(5)"));
    store_word(036, besm6_asm("пино 40(5), мода"));
    store_word(037, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(040, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02000, 00000000000077777ul);
    store_word(02001, 05252525252525252ul);
    store_word(02002, 02525252525252525ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 037u);
    EXPECT_EQ(machine->get_m(3), 0u);
    EXPECT_EQ(machine->get_m(4), 052525u);
    EXPECT_EQ(machine->get_m(5), 0u);
}

//
// Test: VJM instruction (ПВ).
//
TEST_F(dubna_machine, vjm)
{
    // Store the test code.
    store_word(010, besm6_asm("мода, пв 11(2)"));
    store_word(011, besm6_asm("слиа -11(2), пино 23(2)"));
    store_word(012, besm6_asm("пв 13(2), мода"));
    store_word(013, besm6_asm("слиа -13(2), пино 23(2)"));
    store_word(014, besm6_asm("пв 16(2), мода"));
    store_word(015, besm6_asm("мода -1, мода"));
    store_word(016, besm6_asm("уиа 1(3), пио 23(3)"));
    store_word(017, besm6_asm("уиа -1(3), пв 21(2)"));
    store_word(020, besm6_asm("уиа -2(3), мода"));
    store_word(021, besm6_asm("слиа 1(3), пино 23(3)"));
    store_word(022, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(023, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 022u);
    EXPECT_EQ(machine->get_m(2), 020u);
    EXPECT_EQ(machine->get_m(3), 0u);
}

//
// Test: MTJ instruction (УИИ).
//
TEST_F(dubna_machine, mtj)
{
    // Store the test code.
    store_word(010, besm6_asm("мода -15, уиа 16(2)"));
    store_word(011, besm6_asm("слиа -1(2), пино 24(2)"));
    store_word(012, besm6_asm("слиа 1(2), уиа 17(2)"));
    store_word(013, besm6_asm("слиа -17(2), пино 24(2)"));
    store_word(014, besm6_asm("уиа 1(3), сли 2(3)"));
    store_word(015, besm6_asm("слиа -1(2), пино 24(2)"));
    store_word(016, besm6_asm("слиа 1(2), слиа -1(3)"));
    store_word(017, besm6_asm("пино 24(3), слиа 1(3)"));
    store_word(020, besm6_asm("уии 2(3), слиа -1(2)"));
    store_word(021, besm6_asm("пино 24(2), слиа 1(2)"));
    store_word(022, besm6_asm("слиа -1(3), пино 24(3)"));
    store_word(023, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(024, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 023u);
    EXPECT_EQ(machine->get_m(2), 1u);
    EXPECT_EQ(machine->get_m(3), 0u);
}

//
// Test: XTA, UZA, UIA instructions (СЧ, ПО, ПЕ).
//
TEST_F(dubna_machine, xta_uza_u1a)
{
    // Store the test code.
    store_word(010, besm6_asm("сч 2000, по 12"));
    store_word(011, besm6_asm("пб 15, мода"));
    store_word(012, besm6_asm("пе 15, пе 15"));
    store_word(013, besm6_asm("сч 2001, по 15"));
    store_word(014, besm6_asm("по 15, пе 16"));
    store_word(015, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(016, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(02000, 0000000000000000ul);
    store_word(02001, 0000000000000001ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 016u);
    EXPECT_EQ(machine->get_acc(), 1u);
    EXPECT_EQ(machine->get_rmr(), 1u);
}

//
// Test: ATX instruction (ЗП).
//
TEST_F(dubna_machine, atx)
{
    // Store the test code.
    store_word(010, besm6_asm("сч, зп 2000"));
    store_word(011, besm6_asm("зп 2001, зп 2002"));
    store_word(012, besm6_asm("сч 2000, пе 30"));
    store_word(013, besm6_asm("сч 2001, пе 30"));
    store_word(014, besm6_asm("сч 2002, пе 30"));
    store_word(015, besm6_asm("сч 2003, зп 2001"));
    store_word(016, besm6_asm("сч 2000, пе 30"));
    store_word(017, besm6_asm("сч 2001, по 30"));
    store_word(020, besm6_asm("сч 2002, пе 30"));
    store_word(021, besm6_asm("сч 2003, зп 2000"));
    store_word(022, besm6_asm("зп 2002, сч"));
    store_word(023, besm6_asm("зп 2001, сч 2000"));
    store_word(024, besm6_asm("по 30, сч 2001"));
    store_word(025, besm6_asm("пе 30, сч 2002"));
    store_word(026, besm6_asm("по 30, мода"));
    store_word(027, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(030, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02003, 0000000000000001ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 027u);
    EXPECT_EQ(machine->get_acc(), 1u);
    EXPECT_EQ(machine->get_rmr(), 1u);
}

//
// Test: ATI, ITA instructions (УИ, СЧИ).
//
TEST_F(dubna_machine, ati_ita)
{
    // Store the test code.
    store_word(010, besm6_asm("сч, уиа -1(2)"));
    store_word(011, besm6_asm("уи 2, пино 20(2)"));
    store_word(012, besm6_asm("сч 2000, уи 2"));
    store_word(013, besm6_asm("пио 20(2), сч"));
    store_word(014, besm6_asm("счи 2, уи 3"));
    store_word(015, besm6_asm("пио 20(3), слиа 1(3)"));
    store_word(016, besm6_asm("пино 20(3), мода"));
    store_word(017, besm6_asm("стоп 12345(6), мода"));
    store_word(020, besm6_asm("стоп 76543(2), мода"));
    store_word(017, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(020, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02000, 07777777777777777ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 017u);
    EXPECT_EQ(machine->get_acc(), 077777u);
    EXPECT_EQ(machine->get_m(2), 077777u);
    EXPECT_EQ(machine->get_m(3), 0u);
}

//
// Test for instructions ATX 0(0), XTA 0(0), ATI 0(0), ITA 0(0)
// (ЗП 0(0), СЧ 0(0), УИ 0(0), СЧИ 0(0)).
// Address 0 and register m0 should always return 0.
//
TEST_F(dubna_machine, addr0)
{
    // Store the test code.
    store_word(010, besm6_asm("уиа -1(2), счи 2"));
    store_word(011, besm6_asm("зп, мода"));
    store_word(012, besm6_asm("сч, уи 2"));
    store_word(013, besm6_asm("пино 27(2), уиа -1(2)"));
    store_word(014, besm6_asm("счи 2, мода"));
    store_word(015, besm6_asm("зп, сч"));
    store_word(016, besm6_asm("уи 2, пино 27(2)"));
    store_word(017, besm6_asm("уиа -1(2), счи 2"));
    store_word(020, besm6_asm("уи, мода"));
    store_word(021, besm6_asm("счи, уи 2"));
    store_word(022, besm6_asm("пино 27(2), уиа -1(2)"));
    store_word(023, besm6_asm("счи 2, мода"));
    store_word(024, besm6_asm("уи, счи"));
    store_word(025, besm6_asm("уи 2, пино 27(2)"));
    store_word(026, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(027, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 026u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_m(2), 0u);
}

//
// Test: AAX, AOX, AEX instructions (И, ИЛИ, НТЖ).
//
TEST_F(dubna_machine, aax_aox_aex)
{
    // Store the test code.
    store_word(010, besm6_asm("сч 2000, и"));
    store_word(011, besm6_asm("пе 22, сч 2000"));
    store_word(012, besm6_asm("и 2000, нтж 2000"));
    store_word(013, besm6_asm("пе 22, сч 2001"));
    store_word(014, besm6_asm("и 2001, нтж 2001"));
    store_word(015, besm6_asm("пе 22, сч 2001"));
    store_word(016, besm6_asm("и 2002, пе 22"));
    store_word(017, besm6_asm("сч 2001, или 2002"));
    store_word(020, besm6_asm("нтж 2000, пе 22"));
    store_word(021, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(022, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02000, 07777777777777777ul);
    store_word(02001, 05252525252525252ul);
    store_word(02002, 02525252525252525ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 021u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
}

//
// Test: ARX instruction (СЛЦ).
//
TEST_F(dubna_machine, arx)
{
    // Store the test code.
    store_word(010, besm6_asm("сч 2002, слц 2001"));
    store_word(011, besm6_asm("нтж 2003, пе 17"));
    store_word(012, besm6_asm("сч 2000, слц 2001"));
    store_word(013, besm6_asm("нтж 2001, пе 17"));
    store_word(014, besm6_asm("сч 2000, слц 2000"));
    store_word(015, besm6_asm("нтж 2000, пе 17"));
    store_word(016, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(017, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02000, 07777777777777777ul);
    store_word(02001, 00000000000000001ul);
    store_word(02002, 00000000000000013ul);
    store_word(02003, 00000000000000014ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 016u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
}

//
// Test: ITS instruction (СЧИМ).
//
TEST_F(dubna_machine, its)
{
    // Store the test code.
    store_word(010, besm6_asm("уиа 2000(17), счи 17"));
    store_word(011, besm6_asm("нтж 2003, уи 16"));
    store_word(012, besm6_asm("уиа 11(1), уиа 22(2)"));
    store_word(013, besm6_asm("уиа 33(3), счи 1"));
    store_word(014, besm6_asm("счим 2, счим 3"));
    store_word(015, besm6_asm("счим, сли 17(16)"));
    store_word(016, besm6_asm("слиа -2(17), пино 25(17)"));
    store_word(017, besm6_asm("сч 2000, нтж 2004"));
    store_word(020, besm6_asm("пе 25, сч 2001"));
    store_word(021, besm6_asm("нтж 2005, пе 25"));
    store_word(022, besm6_asm("сч 2002, нтж 2006"));
    store_word(023, besm6_asm("пе 25, мода"));
    store_word(024, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(025, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02003, 00000000000077777ul);
    store_word(02004, 00000000000000011ul);
    store_word(02005, 00000000000000022ul);
    store_word(02006, 00000000000000033ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 024u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
    EXPECT_EQ(machine->get_m(1), 011u);
    EXPECT_EQ(machine->get_m(2), 022u);
    EXPECT_EQ(machine->get_m(3), 033u);
}

//
// Test: STI instruction (УИМ).
//
TEST_F(dubna_machine, sti)
{
    // Store the test code.
    store_word(010, besm6_asm("уиа 2004(17), счи 17"));
    store_word(011, besm6_asm("нтж 2004, уи 16"));
    store_word(012, besm6_asm("уим, уим 3"));
    store_word(013, besm6_asm("уим 2, уи 1"));
    store_word(014, besm6_asm("сли 17(16), слиа 4(17)"));
    store_word(015, besm6_asm("пино 31(17), слиа -33(3)"));
    store_word(016, besm6_asm("пино 31(3), слиа -22(2)"));
    store_word(017, besm6_asm("пино 31(2), слиа -11(1)"));
    store_word(020, besm6_asm("пино 31(1), сч 2000"));
    store_word(021, besm6_asm("зп 70776, зп 70777"));
    store_word(022, besm6_asm("сч, уиа 70776(17)"));
    store_word(023, besm6_asm("счм (17), нтж 2000"));
    store_word(024, besm6_asm("пе 31, сч 70776"));
    store_word(025, besm6_asm("пе 31, уиа 17(17)"));
    store_word(026, besm6_asm("сч 2005, уим (17)"));
    store_word(027, besm6_asm("нтж 2000, пе 31"));
    store_word(030, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(031, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02000, 07777777777777777ul);
    store_word(02001, 00000000000000011ul);
    store_word(02002, 00000000000000022ul);
    store_word(02003, 00000000000000033ul);
    store_word(02004, 00000000000077777ul);
    store_word(02005, 00000000000070777ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 030u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
    EXPECT_EQ(machine->get_m(1), 0u);
    EXPECT_EQ(machine->get_m(2), 0u);
    EXPECT_EQ(machine->get_m(3), 0u);
    EXPECT_EQ(machine->get_m(15), 070777u);
}

//
// Test: XTS instruction (СЧМ).
//
TEST_F(dubna_machine, xts)
{
    // Store the test code.
    store_word(010, besm6_asm("уиа 2000(17), счи 17"));
    store_word(011, besm6_asm("нтж 2003, уи 16"));
    store_word(012, besm6_asm("сч 2004, счм 2005"));
    store_word(013, besm6_asm("счм 2006, счм"));
    store_word(014, besm6_asm("сли 17(16), слиа -2(17)"));
    store_word(015, besm6_asm("пино 23(17), сч 2000"));
    store_word(016, besm6_asm("нтж 2004, пе 23"));
    store_word(017, besm6_asm("сч 2001, нтж 2005"));
    store_word(020, besm6_asm("пе 23, сч 2002"));
    store_word(021, besm6_asm("нтж 2006, пе 23"));
    store_word(022, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(023, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02003, 00000000000077777ul);
    store_word(02004, 00000000000000011ul);
    store_word(02005, 00000000000000022ul);
    store_word(02006, 00000000000000033ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 022u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
    EXPECT_EQ(machine->get_m(15), 0u);
}

//
// Test: STX instruction (ЗПМ).
//
TEST_F(dubna_machine, stx)
{
    // Store the test code.
    store_word(010, besm6_asm("уиа 2003(17), счи 17"));
    store_word(011, besm6_asm("нтж 2005, уи 16"));
    store_word(012, besm6_asm("зпм, уи 3"));
    store_word(013, besm6_asm("зпм 2004, уи 2"));
    store_word(014, besm6_asm("зпм 2003, уи 1"));
    store_word(015, besm6_asm("сли 17(16), слиа 4(17)"));
    store_word(016, besm6_asm("пино 27(17), слиа -33(3)"));
    store_word(017, besm6_asm("пино 27(3), слиа -22(2)"));
    store_word(020, besm6_asm("пино 27(2), слиа -11(1)"));
    store_word(021, besm6_asm("пино 27(1), нтж 2000"));
    store_word(022, besm6_asm("пе 27, сч 2003"));
    store_word(023, besm6_asm("нтж 2001, пе 27"));
    store_word(024, besm6_asm("сч 2004, нтж 2002"));
    store_word(025, besm6_asm("пе 27, мода"));
    store_word(026, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(027, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02000, 00000000000000011ul);
    store_word(02001, 00000000000000022ul);
    store_word(02002, 00000000000000033ul);
    store_word(02005, 00000000000077777ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 026u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
    EXPECT_EQ(machine->get_m(1), 0u);
    EXPECT_EQ(machine->get_m(2), 0u);
    EXPECT_EQ(machine->get_m(3), 0u);
    EXPECT_EQ(machine->get_m(15), 0u);
}

//
// Test: ASN, ASX instructions (СД, СДА).
//
TEST_F(dubna_machine, asn_asx)
{
    // Store the test code.
    store_word(010, besm6_asm("уиа -60(14), уиа 60(13)"));
    store_word(011, besm6_asm("сч 2003, сда 77(13)"));
    store_word(012, besm6_asm("нтж 2004(13), пе 33"));
    store_word(013, besm6_asm("слиа -1(13), цикл 11(14)"));
    store_word(014, besm6_asm("уиа -60(14), уиа 60(13)"));
    store_word(015, besm6_asm("сч 2064, сда 20(13)"));
    store_word(016, besm6_asm("нтж 2004(13), пе 33"));
    store_word(017, besm6_asm("слиа -1(13), цикл 15(14)"));
    store_word(020, besm6_asm("сч 2000, сд 2000"));
    store_word(021, besm6_asm("пе 33, сч 2002"));
    store_word(022, besm6_asm("сд 2065, нтж 2001"));
    store_word(023, besm6_asm("пе 33, сч 2000"));
    store_word(024, besm6_asm("сда 64, счмр"));
    store_word(025, besm6_asm("нтж 2067, пе 33"));
    store_word(026, besm6_asm("сч 2000, сда 104"));
    store_word(027, besm6_asm("счмр, нтж 2066"));
    store_word(030, besm6_asm("пе 33, сч 2000"));
    store_word(031, besm6_asm("сд 2070, пе 33"));
    store_word(032, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(033, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02000, 07777777777777777ul);
    store_word(02001, 05252525252525252ul);
    store_word(02002, 02525252525252525ul);
    store_word(02003, 04000000000000000ul);
    store_word(02004, 00000000000000000ul);
    store_word(02005, 04000000000000000ul);
    store_word(02006, 02000000000000000ul);
    store_word(02007, 01000000000000000ul);
    store_word(02010, 00400000000000000ul);
    store_word(02011, 00200000000000000ul);
    store_word(02012, 00100000000000000ul);
    store_word(02013, 00040000000000000ul);
    store_word(02014, 00020000000000000ul);
    store_word(02015, 00010000000000000ul);
    store_word(02016, 00004000000000000ul);
    store_word(02017, 00002000000000000ul);
    store_word(02020, 00001000000000000ul);
    store_word(02021, 00000400000000000ul);
    store_word(02022, 00000200000000000ul);
    store_word(02023, 00000100000000000ul);
    store_word(02024, 00000040000000000ul);
    store_word(02025, 00000020000000000ul);
    store_word(02026, 00000010000000000ul);
    store_word(02027, 00000004000000000ul);
    store_word(02030, 00000002000000000ul);
    store_word(02031, 00000001000000000ul);
    store_word(02032, 00000000400000000ul);
    store_word(02033, 00000000200000000ul);
    store_word(02034, 00000000100000000ul);
    store_word(02035, 00000000040000000ul);
    store_word(02036, 00000000020000000ul);
    store_word(02037, 00000000010000000ul);
    store_word(02040, 00000000004000000ul);
    store_word(02041, 00000000002000000ul);
    store_word(02042, 00000000001000000ul);
    store_word(02043, 00000000000400000ul);
    store_word(02044, 00000000000200000ul);
    store_word(02045, 00000000000100000ul);
    store_word(02046, 00000000000040000ul);
    store_word(02047, 00000000000020000ul);
    store_word(02050, 00000000000010000ul);
    store_word(02051, 00000000000004000ul);
    store_word(02052, 00000000000002000ul);
    store_word(02053, 00000000000001000ul);
    store_word(02054, 00000000000000400ul);
    store_word(02055, 00000000000000200ul);
    store_word(02056, 00000000000000100ul);
    store_word(02057, 00000000000000040ul);
    store_word(02060, 00000000000000020ul);
    store_word(02061, 00000000000000010ul);
    store_word(02062, 00000000000000004ul);
    store_word(02063, 00000000000000002ul);
    store_word(02064, 00000000000000001ul);
    store_word(02065, 03777777777777777ul);
    store_word(02066, 07400000000000000ul);
    store_word(02067, 00000000000007777ul);
    store_word(02070, 00020000000000000ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 032u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
    EXPECT_EQ(machine->get_m(11), 077777u);
    EXPECT_EQ(machine->get_m(12), 0u);
}

//
// Test: ACX, ANX instructions (ЧЕД, НЕД).
//
TEST_F(dubna_machine, acx_anx)
{
    // Store the test code.
    store_word(010, besm6_asm("счи, чед"));
    store_word(011, besm6_asm("пе 35, сч 2000"));
    store_word(012, besm6_asm("чед, нтж 2003"));
    store_word(013, besm6_asm("пе 35, сч 2007"));
    store_word(014, besm6_asm("чед 2004, нтж 2001"));
    store_word(015, besm6_asm("пе 35, уиа -60(14)"));
    store_word(016, besm6_asm("уиа 60(13), уиа 2011(17)"));
    store_word(017, besm6_asm("сч 2001, мода"));
    store_word(020, besm6_asm("пино 21(13), сч"));
    store_word(021, besm6_asm("зп 2010, нед"));
    store_word(022, besm6_asm("счим 13, нтж (17)"));
    store_word(023, besm6_asm("пе 35, сч 2010"));
    store_word(024, besm6_asm("сда 77, счим 13"));
    store_word(025, besm6_asm("и 2002, или (17)"));
    store_word(026, besm6_asm("слиа -1(13), цикл 20(14)"));
    store_word(027, besm6_asm("сч, нед 2000"));
    store_word(030, besm6_asm("нтж 2000, пе 35"));
    store_word(031, besm6_asm("уиа 1001(16), счи 16"));
    store_word(032, besm6_asm("нед 2000, счмр"));
    store_word(033, besm6_asm("нтж 2005, пе 35"));
    store_word(034, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(035, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02000, 07777777777777777ul);
    store_word(02001, 00000000000000001ul);
    store_word(02002, 00000000000000007ul);
    store_word(02003, 00000000000000060ul);
    store_word(02004, 07777777777777750ul);
    store_word(02005, 00010000000000000ul);
    store_word(02006, 05252525252525252ul);
    store_word(02007, 02525252525252525ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 034u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
    EXPECT_EQ(machine->get_m(11), 077777u);
    EXPECT_EQ(machine->get_m(12), 0u);
    EXPECT_EQ(machine->get_m(14), 01001u);
    EXPECT_EQ(machine->get_m(15), 02011u);
}

//
// Test: APX, AUX instructions (СБР, РЗБ).
//
TEST_F(dubna_machine, apx_aux)
{
    // Store the test code.
    store_word(010, besm6_asm("сч 2002, сбр 2000"));
    store_word(011, besm6_asm("рзб 2001, нтж 2003"));
    store_word(012, besm6_asm("пе 22, сч 2002"));
    store_word(013, besm6_asm("сбр 2003, пе 22"));
    store_word(014, besm6_asm("сч 2002, сбр 2002"));
    store_word(015, besm6_asm("рзб 2003, нтж 2003"));
    store_word(016, besm6_asm("пе 22, сч 2000"));
    store_word(017, besm6_asm("рзб 2003, нтж 2003"));
    store_word(020, besm6_asm("пе 22, мода"));
    store_word(021, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(022, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02000, 07777777777777777ul);
    store_word(02001, 03777777777777777ul);
    store_word(02002, 05252525252525252ul);
    store_word(02003, 02525252525252525ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 021u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
}

//
// Test for instructions in stack mode:
//      ATX (15)    ЗП (15)
//      XTA (15)    СЧ (15)
//      WTC (15)    МОД (15)
//      AAX (15)    И (15)
//      ACX (15)    ЧЕД (15)
//      AEX (15)    НТЖ (15)
//      ANX (15)    НЕД (15)
//      AOX (15)    ИЛИ (15)
//      APX (15)    СБР (15)
//      ARX (15)    СЛЦ (15)
//      ASX (15)    СДА (15)
//      AUX (15)    РЗБ (15)
//
TEST_F(dubna_machine, stack)
{
    // Store the test code.
    store_word(0010, besm6_asm("уиа 2010(12), счи 12"));
    store_word(0011, besm6_asm("нтж 2000, уи 12"));
    store_word(0012, besm6_asm("сч, зп 2010"));
    store_word(0013, besm6_asm("зп 2011, зп 2012"));
    store_word(0014, besm6_asm("уиа 2011(17), сч 2000"));
    store_word(0015, besm6_asm("зп (17), сли 17(12)"));
    store_word(0016, besm6_asm("слиа -1(17), пв 102(15)"));
    store_word(0017, besm6_asm("уиа 2007(17), сч"));
    store_word(0020, besm6_asm("зп 1(17), зп 3(17)"));
    store_word(0021, besm6_asm("сч 2000, мода 2"));
    store_word(0022, besm6_asm("зп (17), сли 17(12)"));
    store_word(0023, besm6_asm("слиа 2(17), пв 102(15)"));
    store_word(0024, besm6_asm("сч, зп 2011"));
    store_word(0025, besm6_asm("уиа 2013(17), сч (17)"));
    store_word(0026, besm6_asm("уи 2, сда 130"));
    store_word(0027, besm6_asm("уи 3, сч (17)"));
    store_word(0030, besm6_asm("уи 4, сда 130"));
    store_word(0031, besm6_asm("уи 5, сч (17)"));
    store_word(0032, besm6_asm("уи 6, сда 140"));
    store_word(0033, besm6_asm("уи 7, пв 117(15)"));
    store_word(0034, besm6_asm("уиа 2013(17), мода -1"));
    store_word(0035, besm6_asm("сч (17), уи 6"));
    store_word(0036, besm6_asm("сда 140, уи 7"));
    store_word(0037, besm6_asm("сч -2(17), уи 4"));
    store_word(0040, besm6_asm("сда 140, уи 5"));
    store_word(0041, besm6_asm("сч -3(17), уи 2"));
    store_word(0042, besm6_asm("сда 140, уи 3"));
    store_word(0043, besm6_asm("слиа -3(17), пв 117(15)"));
    store_word(0044, besm6_asm("уиа 1(4), уиа -1(7)"));
    store_word(0045, besm6_asm("уиа -1(3), уиа 2013(17)"));
    store_word(0046, besm6_asm("мод (17), уиа (6)"));
    store_word(0047, besm6_asm("мод (17), уиа (4)"));
    store_word(0050, besm6_asm("мод (17), уиа (2)"));
    store_word(0051, besm6_asm("мода, пв 117(15)"));
    store_word(0052, besm6_asm("уиа 2010(17), сч 2003"));
    store_word(0053, besm6_asm("счм, счм 2004"));
    store_word(0054, besm6_asm("счм 2005, мод -2(17)"));
    store_word(0055, besm6_asm("уиа (2), пино 101(2)"));
    store_word(0056, besm6_asm("сли 17(12), слиа -2(17)"));
    store_word(0057, besm6_asm("пино 101(17), уиа 2010(17)"));
    store_word(0060, besm6_asm("сч 2001, счм 2002"));
    store_word(0061, besm6_asm("и (17), пе 101"));
    store_word(0062, besm6_asm("сч 2001, счм 2002"));
    store_word(0063, besm6_asm("слц (17), счм 2001"));
    store_word(0064, besm6_asm("счм 2002, или (17)"));
    store_word(0065, besm6_asm("нтж (17), пе 101"));
    store_word(0066, besm6_asm("сч 2001, счм 2002"));
    store_word(0067, besm6_asm("счм 2000, сбр (17)"));
    store_word(0070, besm6_asm("рзб (17), нтж 2001"));
    store_word(0071, besm6_asm("пе 101, счм 2000"));
    store_word(0072, besm6_asm("чед (17), нтж 2006"));
    store_word(0073, besm6_asm("пе 101, счм 2000"));
    store_word(0074, besm6_asm("нед (17), нтж 2003"));
    store_word(0075, besm6_asm("пе 101, сч 2000"));
    store_word(0076, besm6_asm("зп (17), сд (17)"));
    store_word(0077, besm6_asm("пе 101, мода"));
    store_word(0100, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(0101, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(0102, besm6_asm("пино 101(17), сч 2010"));
    store_word(0103, besm6_asm("уи 2, пино 101(2)"));
    store_word(0104, besm6_asm("сда 130, уи 2"));
    store_word(0105, besm6_asm("пино 101(2), сч 2012"));
    store_word(0106, besm6_asm("уи 2, пино 101(2)"));
    store_word(0107, besm6_asm("сда 130, уи 2"));
    store_word(0110, besm6_asm("пино 101(2), сч 2011"));
    store_word(0111, besm6_asm("уи 2, сда 130"));
    store_word(0112, besm6_asm("уи 3, слиа 1(2)"));
    store_word(0113, besm6_asm("пино 101(2), слиа 1(3)"));
    store_word(0114, besm6_asm("пино 101(3), сч 2007"));
    store_word(0115, besm6_asm("зп 2010, зп 2011"));
    store_word(0116, besm6_asm("зп 2012, пб (15)"));
    store_word(0117, besm6_asm("сли 17(12), слиа 1(17)"));
    store_word(0120, besm6_asm("пино 101(17), слиа -1(2)"));
    store_word(0121, besm6_asm("пино 101(2), слиа 1(3)"));
    store_word(0122, besm6_asm("пино 101(3), пино 101(4)"));
    store_word(0123, besm6_asm("пино 101(5), слиа -1(6)"));
    store_word(0124, besm6_asm("пино 101(6), слиа 1(7)"));
    store_word(0125, besm6_asm("пино 101(7), пб (15)"));
    store_word(02000, 07777777777777777ul);
    store_word(02001, 05252525252525252ul);
    store_word(02002, 02525252525252525ul);
    store_word(02003, 00000000000000001ul);
    store_word(02004, 00000000000000002ul);
    store_word(02005, 00000000000000003ul);
    store_word(02006, 00000000000000060ul);
    store_word(02007, 07777777700000001ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 0100u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
    EXPECT_EQ(machine->get_m(15), 02010u);
}

//
// Test: XTR, NTR, RТЕ, UZA, U1A, УТА instructions (РЖ, РЖА, СЧРЖ, ПО, ПЕ, СЧМР).
//
TEST_F(dubna_machine, ntr_rte)
{
    // Store the test code.
    store_word(010, besm6_asm("уиа 2052(17), счи"));
    store_word(011, besm6_asm("уиа 77(2), уиа -77(3)"));
    store_word(012, besm6_asm("ржа (2), счрж 77"));
    store_word(013, besm6_asm("зп 2051, счим 2"));
    store_word(014, besm6_asm("сда 27, нтж (17)"));
    store_word(015, besm6_asm("пе 65, пе 65"));
    store_word(016, besm6_asm("рж, счрж 77"));
    store_word(017, besm6_asm("пе 65, рж 2051"));
    store_word(020, besm6_asm("счрж 77, счим 2"));
    store_word(021, besm6_asm("сда 27, нтж (17)"));
    store_word(022, besm6_asm("пе 65, пе 65"));
    store_word(023, besm6_asm("уиа 2052(17), рж (17)"));
    store_word(024, besm6_asm("уиа 2001(17), счрж 77"));
    store_word(025, besm6_asm("счим 2, сда 27"));
    store_word(026, besm6_asm("нтж (17), пе 65"));
    store_word(027, besm6_asm("слиа -1(2), цикл 12(3)"));
    store_word(030, besm6_asm("ржа 77, счрж 41"));
    store_word(031, besm6_asm("нтж 4057, пе 65"));
    store_word(032, besm6_asm("ржа, по 65"));
    store_word(033, besm6_asm("пе 34, пб 65"));
    store_word(034, besm6_asm("ржа 7, пе 65"));
    store_word(035, besm6_asm("ржа 13, по 65"));
    store_word(036, besm6_asm("или, пе 65"));
    store_word(037, besm6_asm("ржа 23, пе 65"));
    store_word(040, besm6_asm("сч 2000, по 65"));
    store_word(041, besm6_asm("ржа 13, пе 65"));
    store_word(042, besm6_asm("ржа 23, по 65"));
    store_word(043, besm6_asm("ржа 30, по 65"));
    store_word(044, besm6_asm("ржа 14, пе 65"));
    store_word(045, besm6_asm("сч 4060, ржа 24"));
    store_word(046, besm6_asm("пе 65, сч 2000"));
    store_word(047, besm6_asm("нтж, сч"));
    store_word(050, besm6_asm("счмр, нтж 2000"));
    store_word(051, besm6_asm("пе 65, слц"));
    store_word(052, besm6_asm("по 65, слц 2000"));
    store_word(053, besm6_asm("пе 65, и 2000"));
    store_word(054, besm6_asm("по 65, мода"));
    store_word(055, besm6_asm("сч, ржа 77"));
    store_word(056, besm6_asm("зп 2051, счрж 77"));
    store_word(057, besm6_asm("нтж 4061, пе 65"));
    store_word(060, besm6_asm("сч 2000, ржа"));
    store_word(061, besm6_asm("сч, по 63"));
    store_word(062, besm6_asm("пб 65, мода"));
    store_word(063, besm6_asm("ржа, сч"));
    store_word(064, besm6_asm("по 66, мода"));
    store_word(065, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(066, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(02000, 07777777777777777ul);
    store_word(04057, 02040000000000000ul);
    store_word(04060, 00000000000000001ul);
    store_word(04061, 03740000000000000ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 066u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
    EXPECT_EQ(machine->get_rau(), 04u);
    EXPECT_EQ(machine->get_m(2), 077777u);
    EXPECT_EQ(machine->get_m(3), 0u);
    EXPECT_EQ(machine->get_m(15), 02001u);
}

//
// Test: YTA instruction (СЧМР).
//
TEST_F(dubna_machine, yta)
{
    // Store the test code.
    store_word(010, besm6_asm("сч 2000, сда 160"));
    store_word(011, besm6_asm("счмр, зп 2002"));
    store_word(012, besm6_asm("счмр, нтж 2000"));
    store_word(013, besm6_asm("пе 70, сч 2002"));
    store_word(014, besm6_asm("нтж 2000, пе 70"));
    store_word(015, besm6_asm("сч 2000, сда 160"));
    store_word(016, besm6_asm("ржа 23, счмр 123"));
    store_word(017, besm6_asm("зп 2002, счмр 65"));
    store_word(020, besm6_asm("нтж 2003, пе 70"));
    store_word(021, besm6_asm("сч 2002, нтж 2004"));
    store_word(022, besm6_asm("пе 70, сч 2000"));
    store_word(023, besm6_asm("нтж 2001, сда 160"));
    store_word(024, besm6_asm("ржа 13, счмр 123"));
    store_word(025, besm6_asm("зп 2002, счмр 65"));
    store_word(026, besm6_asm("нтж 2005, пе 70"));
    store_word(027, besm6_asm("сч 2002, нтж 2006"));
    store_word(030, besm6_asm("пе 70, сч 2000"));
    store_word(031, besm6_asm("сда 160, ржа 3"));
    store_word(032, besm6_asm("счмр 123, зп 2002"));
    store_word(033, besm6_asm("счмр 65, нтж 2003"));
    store_word(034, besm6_asm("пе 70, сч 2002"));
    store_word(035, besm6_asm("нтж 2004, пе 70"));
    store_word(036, besm6_asm("сч 2000, сда 160"));
    store_word(037, besm6_asm("и 2001, счмр"));
    store_word(040, besm6_asm("пе 70, сч 2000"));
    store_word(041, besm6_asm("сда 160, или 2001"));
    store_word(042, besm6_asm("счмр, пе 70"));
    store_word(043, besm6_asm("сч 2000, сда 160"));
    store_word(044, besm6_asm("слц 2001, ржа 7"));
    store_word(045, besm6_asm("счмр, пе 70"));
    store_word(046, besm6_asm("сч 2000, сда 160"));
    store_word(047, besm6_asm("чед 2001, счмр"));
    store_word(050, besm6_asm("пе 70, сч 2000"));
    store_word(051, besm6_asm("сда 160, сбр 2001"));
    store_word(052, besm6_asm("счмр, пе 70"));
    store_word(053, besm6_asm("сч 2000, сда 160"));
    store_word(054, besm6_asm("рзб 2001, счмр"));
    store_word(055, besm6_asm("пе 70, сч 2000"));
    store_word(056, besm6_asm("по 70, счмр"));
    store_word(057, besm6_asm("нтж 2000, пе 70"));
    store_word(060, besm6_asm("и, сч 2000"));
    store_word(061, besm6_asm("пе 62, пб 70"));
    store_word(062, besm6_asm("счмр, нтж 2000"));
    store_word(063, besm6_asm("пе 70, и"));
    store_word(064, besm6_asm("сч 2000, нтж"));
    store_word(065, besm6_asm("счмр, нтж 2000"));
    store_word(066, besm6_asm("пе 70, мода"));
    store_word(067, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(070, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02000, 01234567123456712ul);
    store_word(02001, 07777777777777777ul);
    store_word(02002, 0ul);
    store_word(02003, 00414567123456712ul);
    store_word(02004, 01154567123456712ul);
    store_word(02005, 00403210654321065ul);
    store_word(02006, 01143210654321065ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 067u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
}

//
// Test: Е+N, Е-N, Е+Х, Е-Х instructions (СЛПА, ВЧПА, СЛП, ВЧП).
//
TEST_F(dubna_machine, ean_esn_eax_esx)
{
    // Store the test code.
    store_word(010, besm6_asm("уиа 77602(14), сч 2000"));
    store_word(011, besm6_asm("зп 2003, мода"));
    store_word(012, besm6_asm("сч 2003, слпа 77"));
    store_word(013, besm6_asm("зп 2003, сда 151"));
    store_word(014, besm6_asm("уи 16, сли 16(14)"));
    store_word(015, besm6_asm("пино 34(16), цикл 12(14)"));
    store_word(016, besm6_asm("сч 2003, вчпа 101"));
    store_word(017, besm6_asm("по 34, уиа 77602(14)"));
    store_word(020, besm6_asm("сч 2001, зп 2003"));
    store_word(021, besm6_asm("уиа -1(13), мода"));
    store_word(022, besm6_asm("сч 2003, вчп 2002"));
    store_word(023, besm6_asm("зп 2003, сда 151"));
    store_word(024, besm6_asm("уи 16, сли 16(13)"));
    store_word(025, besm6_asm("пино 34(16), слиа -1(13)"));
    store_word(026, besm6_asm("цикл 22(14), сч 2003"));
    store_word(027, besm6_asm("слп 2001, нтж 2002"));
    store_word(030, besm6_asm("пе 34, сч 2004"));
    store_word(031, besm6_asm("слп 2005, нтж 2006"));
    store_word(032, besm6_asm("пе 34, мода"));
    store_word(033, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(034, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02000, 07750000000000000ul);
    store_word(02001, 00010000000000000ul);
    store_word(02002, 03750000000000000ul);
    store_word(02003, 0ul);
    store_word(02004, 07030000000000000ul);
    store_word(02005, 04010000000000000ul);
    store_word(02006, 06760000000000000ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 033u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
    EXPECT_EQ(machine->get_rau(), 04u);
    EXPECT_EQ(machine->get_m(11), 077600u);
    EXPECT_EQ(machine->get_m(14), 0u);
}

//
// Test: А+Х, А-Х, Х-А instructions (СЛ, ВЧ, ВЧОБ).
//
TEST_F(dubna_machine, aax_asx_xsa)
{
    // Store the test code.
    store_word(010, besm6_asm("уиа 2000(17), ржа 3"));
    store_word(011, besm6_asm("уиа 100(16), счи 16"));
    store_word(012, besm6_asm("вч 2012, по 57"));
    store_word(013, besm6_asm("сл 2013, пе 57"));
    store_word(014, besm6_asm("или, пе 57"));
    store_word(015, besm6_asm("сч 2014, вчоб 2013"));
    store_word(016, besm6_asm("вч 2015, пе 57"));
    store_word(017, besm6_asm("или, пе 57"));
    store_word(020, besm6_asm("сч 2014, счм 2013"));
    store_word(021, besm6_asm("счм 2014, счм 2016"));
    store_word(022, besm6_asm("вч (17), пе 57"));
    store_word(023, besm6_asm("сл (17), вчоб (17)"));
    store_word(024, besm6_asm("пе 57, или"));
    store_word(025, besm6_asm("пе 57, сч 2017"));
    store_word(026, besm6_asm("вч 2020, по 57"));
    store_word(027, besm6_asm("сл 2021, пе 57"));
    store_word(030, besm6_asm("или, по 57"));
    store_word(031, besm6_asm("нтж 2022, пе 57"));
    store_word(032, besm6_asm("сч 2023, вч 2024"));
    store_word(033, besm6_asm("нтж 2025, пе 57"));
    store_word(034, besm6_asm("сч 2024, вч 2023"));
    store_word(035, besm6_asm("нтж 2026, пе 57"));
    store_word(036, besm6_asm("ржа 2, сч 2021"));
    store_word(037, besm6_asm("счм 2027, счм 2021"));
    store_word(040, besm6_asm("счм 2027, сл (17)"));
    store_word(041, besm6_asm("вч (17), вчоб (17)"));
    store_word(042, besm6_asm("пе 57, ржа 2"));
    store_word(043, besm6_asm("сч 2030, вч 2031"));
    store_word(044, besm6_asm("пе 57, нтж 2027"));
    store_word(045, besm6_asm("пе 57, ржа 77"));
    store_word(046, besm6_asm("сч 2032, сл 2032"));
    store_word(047, besm6_asm("ржа, нтж 2033"));
    store_word(050, besm6_asm("пе 57, ржа"));
    store_word(051, besm6_asm("сч 2034, сл 2035"));
    store_word(052, besm6_asm("нтж 2036, пе 57"));
    store_word(053, besm6_asm("сч 2032, вчоб 2037"));
    store_word(054, besm6_asm("счмр 100, нтж 2040"));
    store_word(055, besm6_asm("пе 57, мода"));
    store_word(056, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(057, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail

    store_word(02012, 00000000000000101ul);
    store_word(02013, 00000000000000001ul);
    store_word(02014, 00000000000000002ul);
    store_word(02015, 00037777777777777ul);
    store_word(02016, 00000000000000003ul);
    store_word(02017, 06400000000000100ul);
    store_word(02020, 06400000000000102ul);
    store_word(02021, 04110000000000000ul);
    store_word(02022, 06400000000000000ul);
    store_word(02023, 06420000000000000ul);
    store_word(02024, 06420000000000001ul);
    store_word(02025, 06437777777777777ul);
    store_word(02026, 06400000000000001ul);
    store_word(02027, 04114000000000000ul);
    store_word(02030, 04050000000000000ul);
    store_word(02031, 04060000000000000ul);
    store_word(02032, 00010000000000000ul);
    store_word(02033, 00050000000000000ul);
    store_word(02034, 07700000000001000ul);
    store_word(02035, 04000000000000001ul);
    store_word(02036, 06010000000000001ul);
    store_word(02037, 04010000000000000ul);
    store_word(02040, 03757777777600000ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 056u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
    EXPECT_EQ(machine->get_rau(), 04u);
    EXPECT_EQ(machine->get_m(15), 02000u);
}

//
// Test: AMX instruction (ВЧАБ).
//
TEST_F(dubna_machine, amx)
{
    // Store the test code.
    store_word(010, besm6_asm("уиа 2001(17), ржа 3"));
    store_word(011, besm6_asm("уиа 100(16), счи 16"));
    store_word(012, besm6_asm("вчаб 2013, по 34"));
    store_word(013, besm6_asm("нтж 2000, пе 34"));
    store_word(014, besm6_asm("сч 2000, вчаб 2000"));
    store_word(015, besm6_asm("пе 34, или"));
    store_word(016, besm6_asm("пе 34, сч 2014"));
    store_word(017, besm6_asm("счм 2015, вчаб (17)"));
    store_word(020, besm6_asm("пе 34, нтж 2016"));
    store_word(021, besm6_asm("пе 34, сч 2017"));
    store_word(022, besm6_asm("вчаб 2016, нтж 2020"));
    store_word(023, besm6_asm("пе 34, сч 2021"));
    store_word(024, besm6_asm("счм 2022, вчаб (17)"));
    store_word(025, besm6_asm("нтж 2023, пе 34"));
    store_word(026, besm6_asm("ржа, сч 2024"));
    store_word(027, besm6_asm("счм 2025, вчаб (17)"));
    store_word(030, besm6_asm("нтж 2021, пе 34"));
    store_word(031, besm6_asm("сч 2026, вчаб 2027"));
    store_word(032, besm6_asm("нтж 2030, пе 34"));
    store_word(033, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(034, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02000, 00037777777777777ul);
    store_word(02013, 00000000000000101ul);
    store_word(02014, 00000000000000002ul);
    store_word(02015, 00000000000000003ul);
    store_word(02016, 00000000000000001ul);
    store_word(02017, 00067777777777777ul);
    store_word(02020, 00050000000000000ul);
    store_word(02021, 04050000000000000ul);
    store_word(02022, 06427777777777777ul);
    store_word(02023, 06410000000000000ul);
    store_word(02024, 06410000000000002ul);
    store_word(02025, 06410000000000003ul);
    store_word(02026, 04060000000000000ul);
    store_word(02027, 04057777777777765ul);
    store_word(02030, 01653000000000000ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 033u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
    EXPECT_EQ(machine->get_rau(), 04u);
    EXPECT_EQ(machine->get_m(15), 02001u);
}

//
// Test: AVX instruction (ЗНАК).
//
TEST_F(dubna_machine, avx)
{
    // Store the test code.
    store_word(010, besm6_asm("уиа 2002(17), ржа 3"));
    store_word(011, besm6_asm("уиа 100(16), счи 16"));
    store_word(012, besm6_asm("знак 2000, пе 45"));
    store_word(013, besm6_asm("нтж 2014, пе 45"));
    store_word(014, besm6_asm("счи 16, знак 2001"));
    store_word(015, besm6_asm("по 45, нтж 2015"));
    store_word(016, besm6_asm("пе 45, сч 2001"));
    store_word(017, besm6_asm("знак 2001, пе 45"));
    store_word(020, besm6_asm("нтж 2000, пе 45"));
    store_word(021, besm6_asm("сч 2000, знак 2001"));
    store_word(022, besm6_asm("по 45, нтж 2016"));
    store_word(023, besm6_asm("пе 45, сч 2017"));
    store_word(024, besm6_asm("счм 2020, знак (17)"));
    store_word(025, besm6_asm("пе 45, нтж 2021"));
    store_word(026, besm6_asm("пе 45, ржа"));
    store_word(027, besm6_asm("сч 2001, знак 2001"));
    store_word(030, besm6_asm("пе 45, нтж 2000"));
    store_word(031, besm6_asm("пе 45, сч 2000"));
    store_word(032, besm6_asm("знак 2001, по 45"));
    store_word(033, besm6_asm("нтж 2001, пе 45"));
    store_word(034, besm6_asm("сч 2022, знак 2001"));
    store_word(035, besm6_asm("по 45, нтж 2023"));
    store_word(036, besm6_asm("пе 45, сч 2024"));
    store_word(037, besm6_asm("знак 2001, пе 45"));
    store_word(040, besm6_asm("нтж, пе 45"));
    store_word(041, besm6_asm("сч 2025, знак 2001"));
    store_word(042, besm6_asm("пе 45, нтж 2026"));
    store_word(043, besm6_asm("пе 45, мода"));
    store_word(044, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(045, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02000, 04050000000000000ul);
    store_word(02001, 04020000000000000ul);
    store_word(02014, 00000000000000100ul);
    store_word(02015, 00037777777777700ul);
    store_word(02016, 04070000000000000ul);
    store_word(02017, 04060000000000000ul);
    store_word(02020, 04124000000000000ul);
    store_word(02021, 04114000000000000ul);
    store_word(02022, 07757777777777777ul);
    store_word(02023, 07760000000000001ul);
    store_word(02024, 00010000000000000ul);
    store_word(02025, 00027777777777777ul);
    store_word(02026, 00010000000000001ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 044u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
    EXPECT_EQ(machine->get_rau(), 04u);
    EXPECT_EQ(machine->get_m(15), 02002u);
}

//
// Test: A*X instruction (УМН).
//
TEST_F(dubna_machine, multiply)
{
    // Store the test code.
    store_word(010, besm6_asm("уиа 2001(17), ржа 3"));
    store_word(011, besm6_asm("сч 2013, умн 2014"));
    store_word(012, besm6_asm("зп (17), счмр 100"));
    store_word(013, besm6_asm("зпм 2000, нтж 2015"));
    store_word(014, besm6_asm("пе 37, сч 2000"));
    store_word(015, besm6_asm("нтж 2016, пе 37"));
    store_word(016, besm6_asm("сч 2017, умн 2020"));
    store_word(017, besm6_asm("зп (17), счмр 100"));
    store_word(020, besm6_asm("зпм 2000, нтж 2021"));
    store_word(021, besm6_asm("пе 37, сч 2000"));
    store_word(022, besm6_asm("слпа 130, нтж 2022"));
    store_word(023, besm6_asm("пе 37, ржа"));
    store_word(024, besm6_asm("сч 2023, умн 2024"));
    store_word(025, besm6_asm("нтж 2024, пе 37"));
    store_word(026, besm6_asm("сч 2024, умн 2023"));
    store_word(027, besm6_asm("нтж 2024, пе 37"));
    store_word(030, besm6_asm("сч 2024, умн 2024"));
    store_word(031, besm6_asm("нтж 2023, пе 37"));
    store_word(032, besm6_asm("ржа 2, сч 2025"));
    store_word(033, besm6_asm("умн 2026, зп 2000"));
    store_word(034, besm6_asm("нтж 2026, нтж 2027"));
    store_word(035, besm6_asm("пе 37, мода"));
    store_word(036, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(037, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02013, 06400000000000005);
    store_word(02014, 02400000000000015);
    store_word(02015, 05000000000000000);
    store_word(02016, 05000000000000101);
    store_word(02017, 02400000000000005);
    store_word(02020, 06437777777777763);
    store_word(02021, 05037777777777777);
    store_word(02022, 06417777777777677);
    store_word(02023, 04050000000000000);
    store_word(02024, 04020000000000000);
    store_word(02025, 04110000000000000);
    store_word(02026, 04114000000000000);
    store_word(02027, 00040000000000000);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 036u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
    EXPECT_EQ(machine->get_rau(), 06u);
    EXPECT_EQ(machine->get_m(15), 02001u);
}

//
// Test: A/X instruction (ДЕЛ).
//
TEST_F(dubna_machine, divide)
{
    // Store the test code.
    store_word(010, besm6_asm("уиа 2000(17), ржа 3"));
    store_word(011, besm6_asm("сч 2012, дел 2013"));
    store_word(012, besm6_asm("нтж 2014, пе 14"));
    store_word(013, besm6_asm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_word(014, besm6_asm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_word(02012, 04154000000000000ul);
    store_word(02013, 04114000000000000ul);
    store_word(02014, 04110000000000000ul);

    // Run the code.
    machine->set_pc(010);
    machine->run();

    // Check registers.
    EXPECT_EQ(machine->get_pc(), 013u);
    EXPECT_EQ(machine->get_acc(), 0u);
    EXPECT_EQ(machine->get_rmr(), 0u);
    EXPECT_EQ(machine->get_rau(), 07u);
    EXPECT_EQ(machine->get_m(15), 02000u);
}
