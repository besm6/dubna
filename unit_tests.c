/*
 * Unit tests for SVS processor.
 * Using the CinyTest framework.
 * For details, see: https://github.com/drmonkeysee/CinyTest
 */
#define _DEFAULT_SOURCE
#include <stdlib.h>
#include <unistd.h>
#include "cinytest/ciny.h"
#include "el_master_api.h"
#include "el_svs_api.h"
#include "el_svs_internal.h"

static const char log_filename[] = "test.output";

//
// Mock implementation of the physical memory.
//
static ElMasterWord memory[1024*1024];
static ElMasterTag mem_tag[1024*1024];

//
// Read a word with tag from RAM.
// Mock implementation of the physical memory.
//
ElMasterStatus elMasterRamWordRead(
    ElMasterRamAddress address,
    ElMasterTag *pTag,
    ElMasterWord *pWord)
{
    ct_asserttrue(address < 1024*1024);

    *pWord = memory[address];
    *pTag = mem_tag[address];
    return EMS_OK;
}

//
// Write a word with tag to RAM.
// Mock implementation of the physical memory.
//
ElMasterStatus elMasterRamWordWrite(
    ElMasterRamAddress address,
    ElMasterTag tag,
    ElMasterWord word)
{
    ct_asserttrue(address < 1024*1024);

    memory[address] = word;
    mem_tag[address] = tag;
    return EMS_OK;
}

//
// Write a data word to memory.
//
static void store_data(struct ElSvsProcessor *cpu, unsigned addr, uint64_t val)
{
    elMasterRamWordWrite(addr, 036, val << 16);
}

//
// Write an instruction word to memory.
//
static void store_insn(struct ElSvsProcessor *cpu, unsigned addr, uint64_t val)
{
    elMasterRamWordWrite(addr, 035, val << 16);
}

//
// Fail in case of premature exit.
//
void exit(int status)
{
    for (;;)
        ct_assertfail();
}

//
// Setup for every test.
//
static void setup(void **pcontext)
{
    // Instantiate the processor.
    struct ElSvsProcessor *cpu = ElSvsAllocate(0);
    *pcontext = cpu;

    // Enable full trace to a file.
    ElSvsSetTrace(cpu, "imxr", log_filename);
}

//
// Teardown for every test.
//
static void teardown(void **pcontext)
{
    // Close the trace file.
    struct ElSvsProcessor *cpu = *pcontext;
    ElSvsSetTrace(cpu, "", "");

    // Deallocate the processor.
    free(*pcontext);
    *pcontext = NULL;
}

//
// Test: UJ instruction (ПБ).
//
static void uj(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    //
    //  start   старт   '10'
    //          пб      pass
    //  fail    стоп    '76543'(2)
    //  pass    стоп    '12345'(6)
    //
    store_insn(cpu, 010, ElSvsAsm("00 30 00012, 00 22 00000"));
    store_insn(cpu, 011, ElSvsAsm("02 33 76543, 00 22 00000"));
    store_insn(cpu, 012, ElSvsAsm("06 33 12345, 00 22 00000"));

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check the PC address.
    ct_assertequal(ElSvsGetPC(cpu), 012u);
}

//
// Test: VTM, VZM, V1M instructions (УИА, ПИО, ПИНО).
//
static void vtm_vzm_v1m(void *context)
{
    struct ElSvsProcessor *cpu = context;

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
    store_insn(cpu, 010, ElSvsAsm("уиа (2), пио 12(2)"));
    store_insn(cpu, 011, ElSvsAsm("пб 15, мода"));
    store_insn(cpu, 012, ElSvsAsm("пино 15(2), пино 15(2)"));
    store_insn(cpu, 013, ElSvsAsm("уиа -1(2), пио 15(2)"));
    store_insn(cpu, 014, ElSvsAsm("пио 15(2), пино 16(2)"));
    store_insn(cpu, 015, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_insn(cpu, 016, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 016u);
    ct_assertequal(ElSvsGetM(cpu, 2), 077777u);
}

//
// Test: J+M, UTM instructions (СЛИ, СЛИА).
//
static void jam_utm(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("уиа 1(2), уиа -17(3)"));
    store_insn(cpu, 011, ElSvsAsm("пб 13, мода"));
    store_insn(cpu, 012, ElSvsAsm("сли 2(2), слиа 1(3)"));
    store_insn(cpu, 013, ElSvsAsm("пино 12(2), пино 15(3)"));
    store_insn(cpu, 014, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 015, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 014u);
    ct_assertequal(ElSvsGetM(cpu, 2), 0u);
    ct_assertequal(ElSvsGetM(cpu, 3), 0u);
}

//
// Test: VLM instruction (ЦИКЛ).
//
static void vlm(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 0010, ElSvsAsm("уиа -11(2), уиа -12(3)"));
    store_insn(cpu, 0011, ElSvsAsm("слиа 1(3), цикл 11(2)"));
    store_insn(cpu, 0012, ElSvsAsm("пино 105(2), пино 105(3)"));
    store_insn(cpu, 0013, ElSvsAsm("цикл 105(2), пино 105(2)"));
    store_insn(cpu, 0014, ElSvsAsm("сч 2000, уиа 77401(16)"));
    store_insn(cpu, 0015, ElSvsAsm("зп 2400(16), цикл 15(16)"));
    store_insn(cpu, 0016, ElSvsAsm("сч, уиа 77401(17)"));
    store_insn(cpu, 0017, ElSvsAsm("слц 2400(17), цикл 17(17)"));
    store_insn(cpu, 0020, ElSvsAsm("нтж, по 105"));
    store_insn(cpu, 0021, ElSvsAsm("нтж 2000, пе 105"));
    store_insn(cpu, 0022, ElSvsAsm("уиа 77401(16), уиа 77401(15)"));
    store_insn(cpu, 0023, ElSvsAsm("слц 2400(16), цикл 23(16)"));
    store_insn(cpu, 0024, ElSvsAsm("нтж, по 105"));
    store_insn(cpu, 0025, ElSvsAsm("нтж 2000, пе 105"));
    store_insn(cpu, 0026, ElSvsAsm("слц 2400(15), цикл 26(15)"));
    store_insn(cpu, 0027, ElSvsAsm("нтж, по 105"));
    store_insn(cpu, 0030, ElSvsAsm("нтж 2000, пе 105"));
    store_insn(cpu, 0031, ElSvsAsm("уиа 77401(14), уиа 77401(13)"));
    store_insn(cpu, 0032, ElSvsAsm("слц 2400(14), цикл 32(14)"));
    store_insn(cpu, 0033, ElSvsAsm("нтж, по 105"));
    store_insn(cpu, 0034, ElSvsAsm("нтж 2000, пе 105"));
    store_insn(cpu, 0035, ElSvsAsm("слц 2400(13), цикл 35(13)"));
    store_insn(cpu, 0036, ElSvsAsm("нтж, по 105"));
    store_insn(cpu, 0037, ElSvsAsm("нтж 2000, пе 105"));
    store_insn(cpu, 0040, ElSvsAsm("уиа 77401(12), уиа 77401(11)"));
    store_insn(cpu, 0041, ElSvsAsm("слц 2400(12), цикл 41(12)"));
    store_insn(cpu, 0042, ElSvsAsm("нтж, по 105"));
    store_insn(cpu, 0043, ElSvsAsm("нтж 2000, пе 105"));
    store_insn(cpu, 0044, ElSvsAsm("слц 2400(11), цикл 44(11)"));
    store_insn(cpu, 0045, ElSvsAsm("нтж, по 105"));
    store_insn(cpu, 0046, ElSvsAsm("нтж 2000, пе 105"));
    store_insn(cpu, 0047, ElSvsAsm("уиа 77401(10), уиа 77401(7)"));
    store_insn(cpu, 0050, ElSvsAsm("слц 2400(10), цикл 50(10)"));
    store_insn(cpu, 0051, ElSvsAsm("нтж, по 105"));
    store_insn(cpu, 0052, ElSvsAsm("нтж 2000, пе 105"));
    store_insn(cpu, 0053, ElSvsAsm("слц 2400(7), цикл 53(7)"));
    store_insn(cpu, 0054, ElSvsAsm("нтж, по 105"));
    store_insn(cpu, 0055, ElSvsAsm("нтж 2000, пе 105"));
    store_insn(cpu, 0056, ElSvsAsm("уиа 77401(6), уиа 77401(5)"));
    store_insn(cpu, 0057, ElSvsAsm("слц 2400(6), цикл 57(6)"));
    store_insn(cpu, 0060, ElSvsAsm("нтж, по 105"));
    store_insn(cpu, 0061, ElSvsAsm("нтж 2000, пе 105"));
    store_insn(cpu, 0062, ElSvsAsm("слц 2400(5), цикл 62(5)"));
    store_insn(cpu, 0063, ElSvsAsm("нтж, по 105"));
    store_insn(cpu, 0064, ElSvsAsm("нтж 2000, пе 105"));
    store_insn(cpu, 0065, ElSvsAsm("уиа 77401(4), уиа 77401(3)"));
    store_insn(cpu, 0066, ElSvsAsm("слц 2400(4), цикл 66(4)"));
    store_insn(cpu, 0067, ElSvsAsm("нтж, по 105"));
    store_insn(cpu, 0070, ElSvsAsm("нтж 2000, пе 105"));
    store_insn(cpu, 0071, ElSvsAsm("слц 2400(3), цикл 71(3)"));
    store_insn(cpu, 0072, ElSvsAsm("нтж, по 105"));
    store_insn(cpu, 0073, ElSvsAsm("нтж 2000, пе 105"));
    store_insn(cpu, 0074, ElSvsAsm("уиа 77401(2), мода"));
    store_insn(cpu, 0075, ElSvsAsm("слц 2400(2), цикл 75(2)"));
    store_insn(cpu, 0076, ElSvsAsm("нтж, по 105"));
    store_insn(cpu, 0077, ElSvsAsm("нтж 2000, пе 105"));
    store_insn(cpu, 0100, ElSvsAsm("уиа 77401(1), мода"));
    store_insn(cpu, 0101, ElSvsAsm("слц 2400(1), цикл 101(1)"));
    store_insn(cpu, 0102, ElSvsAsm("нтж, по 105"));
    store_insn(cpu, 0103, ElSvsAsm("нтж 2000, пе 105"));
    store_insn(cpu, 0104, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 0105, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02000, 07777777777777777ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 0104u);
    ct_assertequal(ElSvsGetM(cpu, 1), 0u);
    ct_assertequal(ElSvsGetM(cpu, 2), 0u);
    ct_assertequal(ElSvsGetM(cpu, 3), 0u);
    ct_assertequal(ElSvsGetM(cpu, 4), 0u);
    ct_assertequal(ElSvsGetM(cpu, 5), 0u);
    ct_assertequal(ElSvsGetM(cpu, 6), 0u);
    ct_assertequal(ElSvsGetM(cpu, 7), 0u);
    ct_assertequal(ElSvsGetM(cpu, 8), 0u);
    ct_assertequal(ElSvsGetM(cpu, 9), 0u);
    ct_assertequal(ElSvsGetM(cpu, 10), 0u);
    ct_assertequal(ElSvsGetM(cpu, 11), 0u);
    ct_assertequal(ElSvsGetM(cpu, 12), 0u);
    ct_assertequal(ElSvsGetM(cpu, 13), 0u);
    ct_assertequal(ElSvsGetM(cpu, 14), 0u);
    ct_assertequal(ElSvsGetM(cpu, 15), 0u);
}

//
// Test: UTC, WTC instructions (МОДА, МОД).
//
static void utc_wtc(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("мода -1, уиа (3)"));
    store_insn(cpu, 011, ElSvsAsm("пио 40(3), слиа 1(3)"));
    store_insn(cpu, 012, ElSvsAsm("пино 40(3), мода"));
    store_insn(cpu, 013, ElSvsAsm("мода -1, мода"));
    store_insn(cpu, 014, ElSvsAsm("уиа (3), пио 40(3)"));
    store_insn(cpu, 015, ElSvsAsm("слиа 1(3), пино 40(3)"));
    store_insn(cpu, 016, ElSvsAsm("мод 2000, уиа (3)"));
    store_insn(cpu, 017, ElSvsAsm("пио 40(3), слиа 1(3)"));
    store_insn(cpu, 020, ElSvsAsm("пино 40(3), мод 2000"));
    store_insn(cpu, 021, ElSvsAsm("уиа (3), пио 40(3)"));
    store_insn(cpu, 022, ElSvsAsm("слиа 1(3), пино 40(3)"));
    store_insn(cpu, 023, ElSvsAsm("мода -7, мода 10"));
    store_insn(cpu, 024, ElSvsAsm("уиа -2(3), пио 40(3)"));
    store_insn(cpu, 025, ElSvsAsm("слиа 1(3), пино 40(3)"));
    store_insn(cpu, 026, ElSvsAsm("мод 2000, мода 10"));
    store_insn(cpu, 027, ElSvsAsm("уиа -6(3), слиа -1(3)"));
    store_insn(cpu, 030, ElSvsAsm("пино 40(3), уиа -1(3)"));
    store_insn(cpu, 031, ElSvsAsm("мод 2002(3), уиа (4)"));
    store_insn(cpu, 032, ElSvsAsm("уии 5(4), слиа 52526(5)"));
    store_insn(cpu, 033, ElSvsAsm("пино 40(5), слиа 1(3)"));
    store_insn(cpu, 034, ElSvsAsm("мод 2002(3), уиа (4)"));
    store_insn(cpu, 035, ElSvsAsm("уии 5(4), слиа 25253(5)"));
    store_insn(cpu, 036, ElSvsAsm("пино 40(5), мода"));
    store_insn(cpu, 037, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 040, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02000, 00000000000077777ul);
    store_data(cpu, 02001, 05252525252525252ul);
    store_data(cpu, 02002, 02525252525252525ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 037u);
    ct_assertequal(ElSvsGetM(cpu, 3), 0u);
    ct_assertequal(ElSvsGetM(cpu, 4), 052525u);
    ct_assertequal(ElSvsGetM(cpu, 5), 0u);
}

//
// Test: VJM instruction (ПВ).
//
static void vjm(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("мода, пв 11(2)"));
    store_insn(cpu, 011, ElSvsAsm("слиа -11(2), пино 23(2)"));
    store_insn(cpu, 012, ElSvsAsm("пв 13(2), мода"));
    store_insn(cpu, 013, ElSvsAsm("слиа -13(2), пино 23(2)"));
    store_insn(cpu, 014, ElSvsAsm("пв 16(2), мода"));
    store_insn(cpu, 015, ElSvsAsm("мода -1, мода"));
    store_insn(cpu, 016, ElSvsAsm("уиа 1(3), пио 23(3)"));
    store_insn(cpu, 017, ElSvsAsm("уиа -1(3), пв 21(2)"));
    store_insn(cpu, 020, ElSvsAsm("уиа -2(3), мода"));
    store_insn(cpu, 021, ElSvsAsm("слиа 1(3), пино 23(3)"));
    store_insn(cpu, 022, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 023, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 022u);
    ct_assertequal(ElSvsGetM(cpu, 2), 020u);
    ct_assertequal(ElSvsGetM(cpu, 3), 0u);
}

//
// Test: MTJ instruction (УИИ).
//
static void mtj(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("мода -15, уиа 16(2)"));
    store_insn(cpu, 011, ElSvsAsm("слиа -1(2), пино 24(2)"));
    store_insn(cpu, 012, ElSvsAsm("слиа 1(2), уиа 17(2)"));
    store_insn(cpu, 013, ElSvsAsm("слиа -17(2), пино 24(2)"));
    store_insn(cpu, 014, ElSvsAsm("уиа 1(3), сли 2(3)"));
    store_insn(cpu, 015, ElSvsAsm("слиа -1(2), пино 24(2)"));
    store_insn(cpu, 016, ElSvsAsm("слиа 1(2), слиа -1(3)"));
    store_insn(cpu, 017, ElSvsAsm("пино 24(3), слиа 1(3)"));
    store_insn(cpu, 020, ElSvsAsm("уии 2(3), слиа -1(2)"));
    store_insn(cpu, 021, ElSvsAsm("пино 24(2), слиа 1(2)"));
    store_insn(cpu, 022, ElSvsAsm("слиа -1(3), пино 24(3)"));
    store_insn(cpu, 023, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 024, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 023u);
    ct_assertequal(ElSvsGetM(cpu, 2), 1u);
    ct_assertequal(ElSvsGetM(cpu, 3), 0u);
}

//
// Test: XTA, UZA, UIA instructions (СЧ, ПО, ПЕ).
//
static void xta_uza_u1a(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("сч 2000, по 12"));
    store_insn(cpu, 011, ElSvsAsm("пб 15, мода"));
    store_insn(cpu, 012, ElSvsAsm("пе 15, пе 15"));
    store_insn(cpu, 013, ElSvsAsm("сч 2001, по 15"));
    store_insn(cpu, 014, ElSvsAsm("по 15, пе 16"));
    store_insn(cpu, 015, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_insn(cpu, 016, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_data(cpu, 02000, 0000000000000000ul);
    store_data(cpu, 02001, 0000000000000001ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 016u);
    ct_assertequal(ElSvsGetAcc(cpu), 1u);
    ct_assertequal(ElSvsGetRMR(cpu), 1u);
}

//
// Test: ATX instruction (ЗП).
//
static void atx(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("сч, зп 2000"));
    store_insn(cpu, 011, ElSvsAsm("зп 2001, зп 2002"));
    store_insn(cpu, 012, ElSvsAsm("сч 2000, пе 30"));
    store_insn(cpu, 013, ElSvsAsm("сч 2001, пе 30"));
    store_insn(cpu, 014, ElSvsAsm("сч 2002, пе 30"));
    store_insn(cpu, 015, ElSvsAsm("сч 2003, зп 2001"));
    store_insn(cpu, 016, ElSvsAsm("сч 2000, пе 30"));
    store_insn(cpu, 017, ElSvsAsm("сч 2001, по 30"));
    store_insn(cpu, 020, ElSvsAsm("сч 2002, пе 30"));
    store_insn(cpu, 021, ElSvsAsm("сч 2003, зп 2000"));
    store_insn(cpu, 022, ElSvsAsm("зп 2002, сч"));
    store_insn(cpu, 023, ElSvsAsm("зп 2001, сч 2000"));
    store_insn(cpu, 024, ElSvsAsm("по 30, сч 2001"));
    store_insn(cpu, 025, ElSvsAsm("пе 30, сч 2002"));
    store_insn(cpu, 026, ElSvsAsm("по 30, мода"));
    store_insn(cpu, 027, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 030, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02003, 0000000000000001ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 027u);
    ct_assertequal(ElSvsGetAcc(cpu), 1u);
    ct_assertequal(ElSvsGetRMR(cpu), 1u);
}

//
// Test: ATI, ITA instructions (УИ, СЧИ).
//
static void ati_ita(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("сч, уиа -1(2)"));
    store_insn(cpu, 011, ElSvsAsm("уи 2, пино 20(2)"));
    store_insn(cpu, 012, ElSvsAsm("сч 2000, уи 2"));
    store_insn(cpu, 013, ElSvsAsm("пио 20(2), сч"));
    store_insn(cpu, 014, ElSvsAsm("счи 2, уи 3"));
    store_insn(cpu, 015, ElSvsAsm("пио 20(3), слиа 1(3)"));
    store_insn(cpu, 016, ElSvsAsm("пино 20(3), мода"));
    store_insn(cpu, 017, ElSvsAsm("стоп 12345(6), мода"));
    store_insn(cpu, 020, ElSvsAsm("стоп 76543(2), мода"));
    store_insn(cpu, 017, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 020, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02000, 07777777777777777ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 017u);
    ct_assertequal(ElSvsGetAcc(cpu), 077777u);
    ct_assertequal(ElSvsGetM(cpu, 2), 077777u);
    ct_assertequal(ElSvsGetM(cpu, 3), 0u);
}

//
// Test for instructions ATX 0(0), XTA 0(0), ATI 0(0), ITA 0(0)
// (ЗП 0(0), СЧ 0(0), УИ 0(0), СЧИ 0(0)).
// Address 0 and register m0 should always return 0.
//
static void addr0(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("уиа -1(2), счи 2"));
    store_insn(cpu, 011, ElSvsAsm("зп, мода"));
    store_insn(cpu, 012, ElSvsAsm("сч, уи 2"));
    store_insn(cpu, 013, ElSvsAsm("пино 27(2), уиа -1(2)"));
    store_insn(cpu, 014, ElSvsAsm("счи 2, мода"));
    store_insn(cpu, 015, ElSvsAsm("зп, сч"));
    store_insn(cpu, 016, ElSvsAsm("уи 2, пино 27(2)"));
    store_insn(cpu, 017, ElSvsAsm("уиа -1(2), счи 2"));
    store_insn(cpu, 020, ElSvsAsm("уи, мода"));
    store_insn(cpu, 021, ElSvsAsm("счи, уи 2"));
    store_insn(cpu, 022, ElSvsAsm("пино 27(2), уиа -1(2)"));
    store_insn(cpu, 023, ElSvsAsm("счи 2, мода"));
    store_insn(cpu, 024, ElSvsAsm("уи, счи"));
    store_insn(cpu, 025, ElSvsAsm("уи 2, пино 27(2)"));
    store_insn(cpu, 026, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 027, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 026u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetM(cpu, 2), 0u);
}

//
// Test: AAX, AOX, AEX instructions (И, ИЛИ, НТЖ).
//
static void aax_aox_aex(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("сч 2000, и"));
    store_insn(cpu, 011, ElSvsAsm("пе 22, сч 2000"));
    store_insn(cpu, 012, ElSvsAsm("и 2000, нтж 2000"));
    store_insn(cpu, 013, ElSvsAsm("пе 22, сч 2001"));
    store_insn(cpu, 014, ElSvsAsm("и 2001, нтж 2001"));
    store_insn(cpu, 015, ElSvsAsm("пе 22, сч 2001"));
    store_insn(cpu, 016, ElSvsAsm("и 2002, пе 22"));
    store_insn(cpu, 017, ElSvsAsm("сч 2001, или 2002"));
    store_insn(cpu, 020, ElSvsAsm("нтж 2000, пе 22"));
    store_insn(cpu, 021, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 022, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02000, 07777777777777777ul);
    store_data(cpu, 02001, 05252525252525252ul);
    store_data(cpu, 02002, 02525252525252525ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 021u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
}

//
// Test: ARX instruction (СЛЦ).
//
static void arx(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("сч 2002, слц 2001"));
    store_insn(cpu, 011, ElSvsAsm("нтж 2003, пе 17"));
    store_insn(cpu, 012, ElSvsAsm("сч 2000, слц 2001"));
    store_insn(cpu, 013, ElSvsAsm("нтж 2001, пе 17"));
    store_insn(cpu, 014, ElSvsAsm("сч 2000, слц 2000"));
    store_insn(cpu, 015, ElSvsAsm("нтж 2000, пе 17"));
    store_insn(cpu, 016, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 017, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02000, 07777777777777777ul);
    store_data(cpu, 02001, 00000000000000001ul);
    store_data(cpu, 02002, 00000000000000013ul);
    store_data(cpu, 02003, 00000000000000014ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 016u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
}

//
// Test: ITS instruction (СЧИМ).
//
static void its(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("уиа 2000(17), счи 17"));
    store_insn(cpu, 011, ElSvsAsm("нтж 2003, уи 16"));
    store_insn(cpu, 012, ElSvsAsm("уиа 11(1), уиа 22(2)"));
    store_insn(cpu, 013, ElSvsAsm("уиа 33(3), счи 1"));
    store_insn(cpu, 014, ElSvsAsm("счим 2, счим 3"));
    store_insn(cpu, 015, ElSvsAsm("счим, сли 17(16)"));
    store_insn(cpu, 016, ElSvsAsm("слиа -2(17), пино 25(17)"));
    store_insn(cpu, 017, ElSvsAsm("сч 2000, нтж 2004"));
    store_insn(cpu, 020, ElSvsAsm("пе 25, сч 2001"));
    store_insn(cpu, 021, ElSvsAsm("нтж 2005, пе 25"));
    store_insn(cpu, 022, ElSvsAsm("сч 2002, нтж 2006"));
    store_insn(cpu, 023, ElSvsAsm("пе 25, мода"));
    store_insn(cpu, 024, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 025, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02003, 00000000000077777ul);
    store_data(cpu, 02004, 00000000000000011ul);
    store_data(cpu, 02005, 00000000000000022ul);
    store_data(cpu, 02006, 00000000000000033ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 024u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
    ct_assertequal(ElSvsGetM(cpu, 1), 011u);
    ct_assertequal(ElSvsGetM(cpu, 2), 022u);
    ct_assertequal(ElSvsGetM(cpu, 3), 033u);
}

//
// Test: STI instruction (УИМ).
//
static void sti(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("уиа 2004(17), счи 17"));
    store_insn(cpu, 011, ElSvsAsm("нтж 2004, уи 16"));
    store_insn(cpu, 012, ElSvsAsm("уим, уим 3"));
    store_insn(cpu, 013, ElSvsAsm("уим 2, уи 1"));
    store_insn(cpu, 014, ElSvsAsm("сли 17(16), слиа 4(17)"));
    store_insn(cpu, 015, ElSvsAsm("пино 31(17), слиа -33(3)"));
    store_insn(cpu, 016, ElSvsAsm("пино 31(3), слиа -22(2)"));
    store_insn(cpu, 017, ElSvsAsm("пино 31(2), слиа -11(1)"));
    store_insn(cpu, 020, ElSvsAsm("пино 31(1), сч 2000"));
    store_insn(cpu, 021, ElSvsAsm("зп 70776, зп 70777"));
    store_insn(cpu, 022, ElSvsAsm("сч, уиа 70776(17)"));
    store_insn(cpu, 023, ElSvsAsm("счм (17), нтж 2000"));
    store_insn(cpu, 024, ElSvsAsm("пе 31, сч 70776"));
    store_insn(cpu, 025, ElSvsAsm("пе 31, уиа 17(17)"));
    store_insn(cpu, 026, ElSvsAsm("сч 2005, уим (17)"));
    store_insn(cpu, 027, ElSvsAsm("нтж 2000, пе 31"));
    store_insn(cpu, 030, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 031, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02000, 07777777777777777ul);
    store_data(cpu, 02001, 00000000000000011ul);
    store_data(cpu, 02002, 00000000000000022ul);
    store_data(cpu, 02003, 00000000000000033ul);
    store_data(cpu, 02004, 00000000000077777ul);
    store_data(cpu, 02005, 00000000000070777ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 030u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
    ct_assertequal(ElSvsGetM(cpu, 1), 0u);
    ct_assertequal(ElSvsGetM(cpu, 2), 0u);
    ct_assertequal(ElSvsGetM(cpu, 3), 0u);
    ct_assertequal(ElSvsGetM(cpu, 15), 070777u);
}

//
// Test: XTS instruction (СЧМ).
//
static void xts(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("уиа 2000(17), счи 17"));
    store_insn(cpu, 011, ElSvsAsm("нтж 2003, уи 16"));
    store_insn(cpu, 012, ElSvsAsm("сч 2004, счм 2005"));
    store_insn(cpu, 013, ElSvsAsm("счм 2006, счм"));
    store_insn(cpu, 014, ElSvsAsm("сли 17(16), слиа -2(17)"));
    store_insn(cpu, 015, ElSvsAsm("пино 23(17), сч 2000"));
    store_insn(cpu, 016, ElSvsAsm("нтж 2004, пе 23"));
    store_insn(cpu, 017, ElSvsAsm("сч 2001, нтж 2005"));
    store_insn(cpu, 020, ElSvsAsm("пе 23, сч 2002"));
    store_insn(cpu, 021, ElSvsAsm("нтж 2006, пе 23"));
    store_insn(cpu, 022, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 023, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02003, 00000000000077777ul);
    store_data(cpu, 02004, 00000000000000011ul);
    store_data(cpu, 02005, 00000000000000022ul);
    store_data(cpu, 02006, 00000000000000033ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 022u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
    ct_assertequal(ElSvsGetM(cpu, 15), 0u);
}

//
// Test: STX instruction (ЗПМ).
//
static void stx(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("уиа 2003(17), счи 17"));
    store_insn(cpu, 011, ElSvsAsm("нтж 2005, уи 16"));
    store_insn(cpu, 012, ElSvsAsm("зпм, уи 3"));
    store_insn(cpu, 013, ElSvsAsm("зпм 2004, уи 2"));
    store_insn(cpu, 014, ElSvsAsm("зпм 2003, уи 1"));
    store_insn(cpu, 015, ElSvsAsm("сли 17(16), слиа 4(17)"));
    store_insn(cpu, 016, ElSvsAsm("пино 27(17), слиа -33(3)"));
    store_insn(cpu, 017, ElSvsAsm("пино 27(3), слиа -22(2)"));
    store_insn(cpu, 020, ElSvsAsm("пино 27(2), слиа -11(1)"));
    store_insn(cpu, 021, ElSvsAsm("пино 27(1), нтж 2000"));
    store_insn(cpu, 022, ElSvsAsm("пе 27, сч 2003"));
    store_insn(cpu, 023, ElSvsAsm("нтж 2001, пе 27"));
    store_insn(cpu, 024, ElSvsAsm("сч 2004, нтж 2002"));
    store_insn(cpu, 025, ElSvsAsm("пе 27, мода"));
    store_insn(cpu, 026, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 027, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02000, 00000000000000011ul);
    store_data(cpu, 02001, 00000000000000022ul);
    store_data(cpu, 02002, 00000000000000033ul);
    store_data(cpu, 02005, 00000000000077777ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 026u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
    ct_assertequal(ElSvsGetM(cpu, 1), 0u);
    ct_assertequal(ElSvsGetM(cpu, 2), 0u);
    ct_assertequal(ElSvsGetM(cpu, 3), 0u);
    ct_assertequal(ElSvsGetM(cpu, 15), 0u);
}

//
// Test: ASN, ASX instructions (СД, СДА).
//
static void asn_asx(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("уиа -60(14), уиа 60(13)"));
    store_insn(cpu, 011, ElSvsAsm("сч 2003, сда 77(13)"));
    store_insn(cpu, 012, ElSvsAsm("нтж 2004(13), пе 33"));
    store_insn(cpu, 013, ElSvsAsm("слиа -1(13), цикл 11(14)"));
    store_insn(cpu, 014, ElSvsAsm("уиа -60(14), уиа 60(13)"));
    store_insn(cpu, 015, ElSvsAsm("сч 2064, сда 20(13)"));
    store_insn(cpu, 016, ElSvsAsm("нтж 2004(13), пе 33"));
    store_insn(cpu, 017, ElSvsAsm("слиа -1(13), цикл 15(14)"));
    store_insn(cpu, 020, ElSvsAsm("сч 2000, сд 2000"));
    store_insn(cpu, 021, ElSvsAsm("пе 33, сч 2002"));
    store_insn(cpu, 022, ElSvsAsm("сд 2065, нтж 2001"));
    store_insn(cpu, 023, ElSvsAsm("пе 33, сч 2000"));
    store_insn(cpu, 024, ElSvsAsm("сда 64, счмр"));
    store_insn(cpu, 025, ElSvsAsm("нтж 2067, пе 33"));
    store_insn(cpu, 026, ElSvsAsm("сч 2000, сда 104"));
    store_insn(cpu, 027, ElSvsAsm("счмр, нтж 2066"));
    store_insn(cpu, 030, ElSvsAsm("пе 33, сч 2000"));
    store_insn(cpu, 031, ElSvsAsm("сд 2070, пе 33"));
    store_insn(cpu, 032, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 033, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02000, 07777777777777777ul);
    store_data(cpu, 02001, 05252525252525252ul);
    store_data(cpu, 02002, 02525252525252525ul);
    store_data(cpu, 02003, 04000000000000000ul);
    store_data(cpu, 02004, 00000000000000000ul);
    store_data(cpu, 02005, 04000000000000000ul);
    store_data(cpu, 02006, 02000000000000000ul);
    store_data(cpu, 02007, 01000000000000000ul);
    store_data(cpu, 02010, 00400000000000000ul);
    store_data(cpu, 02011, 00200000000000000ul);
    store_data(cpu, 02012, 00100000000000000ul);
    store_data(cpu, 02013, 00040000000000000ul);
    store_data(cpu, 02014, 00020000000000000ul);
    store_data(cpu, 02015, 00010000000000000ul);
    store_data(cpu, 02016, 00004000000000000ul);
    store_data(cpu, 02017, 00002000000000000ul);
    store_data(cpu, 02020, 00001000000000000ul);
    store_data(cpu, 02021, 00000400000000000ul);
    store_data(cpu, 02022, 00000200000000000ul);
    store_data(cpu, 02023, 00000100000000000ul);
    store_data(cpu, 02024, 00000040000000000ul);
    store_data(cpu, 02025, 00000020000000000ul);
    store_data(cpu, 02026, 00000010000000000ul);
    store_data(cpu, 02027, 00000004000000000ul);
    store_data(cpu, 02030, 00000002000000000ul);
    store_data(cpu, 02031, 00000001000000000ul);
    store_data(cpu, 02032, 00000000400000000ul);
    store_data(cpu, 02033, 00000000200000000ul);
    store_data(cpu, 02034, 00000000100000000ul);
    store_data(cpu, 02035, 00000000040000000ul);
    store_data(cpu, 02036, 00000000020000000ul);
    store_data(cpu, 02037, 00000000010000000ul);
    store_data(cpu, 02040, 00000000004000000ul);
    store_data(cpu, 02041, 00000000002000000ul);
    store_data(cpu, 02042, 00000000001000000ul);
    store_data(cpu, 02043, 00000000000400000ul);
    store_data(cpu, 02044, 00000000000200000ul);
    store_data(cpu, 02045, 00000000000100000ul);
    store_data(cpu, 02046, 00000000000040000ul);
    store_data(cpu, 02047, 00000000000020000ul);
    store_data(cpu, 02050, 00000000000010000ul);
    store_data(cpu, 02051, 00000000000004000ul);
    store_data(cpu, 02052, 00000000000002000ul);
    store_data(cpu, 02053, 00000000000001000ul);
    store_data(cpu, 02054, 00000000000000400ul);
    store_data(cpu, 02055, 00000000000000200ul);
    store_data(cpu, 02056, 00000000000000100ul);
    store_data(cpu, 02057, 00000000000000040ul);
    store_data(cpu, 02060, 00000000000000020ul);
    store_data(cpu, 02061, 00000000000000010ul);
    store_data(cpu, 02062, 00000000000000004ul);
    store_data(cpu, 02063, 00000000000000002ul);
    store_data(cpu, 02064, 00000000000000001ul);
    store_data(cpu, 02065, 03777777777777777ul);
    store_data(cpu, 02066, 07400000000000000ul);
    store_data(cpu, 02067, 00000000000007777ul);
    store_data(cpu, 02070, 00020000000000000ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 032u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
    ct_assertequal(ElSvsGetM(cpu, 11), 077777u);
    ct_assertequal(ElSvsGetM(cpu, 12), 0u);
}

//
// Test: ACX, ANX instructions (ЧЕД, НЕД).
//
static void acx_anx(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("счи, чед"));
    store_insn(cpu, 011, ElSvsAsm("пе 35, сч 2000"));
    store_insn(cpu, 012, ElSvsAsm("чед, нтж 2003"));
    store_insn(cpu, 013, ElSvsAsm("пе 35, сч 2007"));
    store_insn(cpu, 014, ElSvsAsm("чед 2004, нтж 2001"));
    store_insn(cpu, 015, ElSvsAsm("пе 35, уиа -60(14)"));
    store_insn(cpu, 016, ElSvsAsm("уиа 60(13), уиа 2011(17)"));
    store_insn(cpu, 017, ElSvsAsm("сч 2001, мода"));
    store_insn(cpu, 020, ElSvsAsm("пино 21(13), сч"));
    store_insn(cpu, 021, ElSvsAsm("зп 2010, нед"));
    store_insn(cpu, 022, ElSvsAsm("счим 13, нтж (17)"));
    store_insn(cpu, 023, ElSvsAsm("пе 35, сч 2010"));
    store_insn(cpu, 024, ElSvsAsm("сда 77, счим 13"));
    store_insn(cpu, 025, ElSvsAsm("и 2002, или (17)"));
    store_insn(cpu, 026, ElSvsAsm("слиа -1(13), цикл 20(14)"));
    store_insn(cpu, 027, ElSvsAsm("сч, нед 2000"));
    store_insn(cpu, 030, ElSvsAsm("нтж 2000, пе 35"));
    store_insn(cpu, 031, ElSvsAsm("уиа 1001(16), счи 16"));
    store_insn(cpu, 032, ElSvsAsm("нед 2000, счмр"));
    store_insn(cpu, 033, ElSvsAsm("нтж 2005, пе 35"));
    store_insn(cpu, 034, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 035, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02000, 07777777777777777ul);
    store_data(cpu, 02001, 00000000000000001ul);
    store_data(cpu, 02002, 00000000000000007ul);
    store_data(cpu, 02003, 00000000000000060ul);
    store_data(cpu, 02004, 07777777777777750ul);
    store_data(cpu, 02005, 00010000000000000ul);
    store_data(cpu, 02006, 05252525252525252ul);
    store_data(cpu, 02007, 02525252525252525ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 034u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
    ct_assertequal(ElSvsGetM(cpu, 11), 077777u);
    ct_assertequal(ElSvsGetM(cpu, 12), 0u);
    ct_assertequal(ElSvsGetM(cpu, 14), 01001u);
    ct_assertequal(ElSvsGetM(cpu, 15), 02011u);
}

//
// Test: APX, AUX instructions (СБР, РЗБ).
//
static void apx_aux(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("сч 2002, сбр 2000"));
    store_insn(cpu, 011, ElSvsAsm("рзб 2001, нтж 2003"));
    store_insn(cpu, 012, ElSvsAsm("пе 22, сч 2002"));
    store_insn(cpu, 013, ElSvsAsm("сбр 2003, пе 22"));
    store_insn(cpu, 014, ElSvsAsm("сч 2002, сбр 2002"));
    store_insn(cpu, 015, ElSvsAsm("рзб 2003, нтж 2003"));
    store_insn(cpu, 016, ElSvsAsm("пе 22, сч 2000"));
    store_insn(cpu, 017, ElSvsAsm("рзб 2003, нтж 2003"));
    store_insn(cpu, 020, ElSvsAsm("пе 22, мода"));
    store_insn(cpu, 021, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 022, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02000, 07777777777777777ul);
    store_data(cpu, 02001, 03777777777777777ul);
    store_data(cpu, 02002, 05252525252525252ul);
    store_data(cpu, 02003, 02525252525252525ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 021u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
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
static void stack(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 0010, ElSvsAsm("уиа 2010(12), счи 12"));
    store_insn(cpu, 0011, ElSvsAsm("нтж 2000, уи 12"));
    store_insn(cpu, 0012, ElSvsAsm("сч, зп 2010"));
    store_insn(cpu, 0013, ElSvsAsm("зп 2011, зп 2012"));
    store_insn(cpu, 0014, ElSvsAsm("уиа 2011(17), сч 2000"));
    store_insn(cpu, 0015, ElSvsAsm("зп (17), сли 17(12)"));
    store_insn(cpu, 0016, ElSvsAsm("слиа -1(17), пв 102(15)"));
    store_insn(cpu, 0017, ElSvsAsm("уиа 2007(17), сч"));
    store_insn(cpu, 0020, ElSvsAsm("зп 1(17), зп 3(17)"));
    store_insn(cpu, 0021, ElSvsAsm("сч 2000, мода 2"));
    store_insn(cpu, 0022, ElSvsAsm("зп (17), сли 17(12)"));
    store_insn(cpu, 0023, ElSvsAsm("слиа 2(17), пв 102(15)"));
    store_insn(cpu, 0024, ElSvsAsm("сч, зп 2011"));
    store_insn(cpu, 0025, ElSvsAsm("уиа 2013(17), сч (17)"));
    store_insn(cpu, 0026, ElSvsAsm("уи 2, сда 130"));
    store_insn(cpu, 0027, ElSvsAsm("уи 3, сч (17)"));
    store_insn(cpu, 0030, ElSvsAsm("уи 4, сда 130"));
    store_insn(cpu, 0031, ElSvsAsm("уи 5, сч (17)"));
    store_insn(cpu, 0032, ElSvsAsm("уи 6, сда 140"));
    store_insn(cpu, 0033, ElSvsAsm("уи 7, пв 117(15)"));
    store_insn(cpu, 0034, ElSvsAsm("уиа 2013(17), мода -1"));
    store_insn(cpu, 0035, ElSvsAsm("сч (17), уи 6"));
    store_insn(cpu, 0036, ElSvsAsm("сда 140, уи 7"));
    store_insn(cpu, 0037, ElSvsAsm("сч -2(17), уи 4"));
    store_insn(cpu, 0040, ElSvsAsm("сда 140, уи 5"));
    store_insn(cpu, 0041, ElSvsAsm("сч -3(17), уи 2"));
    store_insn(cpu, 0042, ElSvsAsm("сда 140, уи 3"));
    store_insn(cpu, 0043, ElSvsAsm("слиа -3(17), пв 117(15)"));
    store_insn(cpu, 0044, ElSvsAsm("уиа 1(4), уиа -1(7)"));
    store_insn(cpu, 0045, ElSvsAsm("уиа -1(3), уиа 2013(17)"));
    store_insn(cpu, 0046, ElSvsAsm("мод (17), уиа (6)"));
    store_insn(cpu, 0047, ElSvsAsm("мод (17), уиа (4)"));
    store_insn(cpu, 0050, ElSvsAsm("мод (17), уиа (2)"));
    store_insn(cpu, 0051, ElSvsAsm("мода, пв 117(15)"));
    store_insn(cpu, 0052, ElSvsAsm("уиа 2010(17), сч 2003"));
    store_insn(cpu, 0053, ElSvsAsm("счм, счм 2004"));
    store_insn(cpu, 0054, ElSvsAsm("счм 2005, мод -2(17)"));
    store_insn(cpu, 0055, ElSvsAsm("уиа (2), пино 101(2)"));
    store_insn(cpu, 0056, ElSvsAsm("сли 17(12), слиа -2(17)"));
    store_insn(cpu, 0057, ElSvsAsm("пино 101(17), уиа 2010(17)"));
    store_insn(cpu, 0060, ElSvsAsm("сч 2001, счм 2002"));
    store_insn(cpu, 0061, ElSvsAsm("и (17), пе 101"));
    store_insn(cpu, 0062, ElSvsAsm("сч 2001, счм 2002"));
    store_insn(cpu, 0063, ElSvsAsm("слц (17), счм 2001"));
    store_insn(cpu, 0064, ElSvsAsm("счм 2002, или (17)"));
    store_insn(cpu, 0065, ElSvsAsm("нтж (17), пе 101"));
    store_insn(cpu, 0066, ElSvsAsm("сч 2001, счм 2002"));
    store_insn(cpu, 0067, ElSvsAsm("счм 2000, сбр (17)"));
    store_insn(cpu, 0070, ElSvsAsm("рзб (17), нтж 2001"));
    store_insn(cpu, 0071, ElSvsAsm("пе 101, счм 2000"));
    store_insn(cpu, 0072, ElSvsAsm("чед (17), нтж 2006"));
    store_insn(cpu, 0073, ElSvsAsm("пе 101, счм 2000"));
    store_insn(cpu, 0074, ElSvsAsm("нед (17), нтж 2003"));
    store_insn(cpu, 0075, ElSvsAsm("пе 101, сч 2000"));
    store_insn(cpu, 0076, ElSvsAsm("зп (17), сд (17)"));
    store_insn(cpu, 0077, ElSvsAsm("пе 101, мода"));
    store_insn(cpu, 0100, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 0101, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_insn(cpu, 0102, ElSvsAsm("пино 101(17), сч 2010"));
    store_insn(cpu, 0103, ElSvsAsm("уи 2, пино 101(2)"));
    store_insn(cpu, 0104, ElSvsAsm("сда 130, уи 2"));
    store_insn(cpu, 0105, ElSvsAsm("пино 101(2), сч 2012"));
    store_insn(cpu, 0106, ElSvsAsm("уи 2, пино 101(2)"));
    store_insn(cpu, 0107, ElSvsAsm("сда 130, уи 2"));
    store_insn(cpu, 0110, ElSvsAsm("пино 101(2), сч 2011"));
    store_insn(cpu, 0111, ElSvsAsm("уи 2, сда 130"));
    store_insn(cpu, 0112, ElSvsAsm("уи 3, слиа 1(2)"));
    store_insn(cpu, 0113, ElSvsAsm("пино 101(2), слиа 1(3)"));
    store_insn(cpu, 0114, ElSvsAsm("пино 101(3), сч 2007"));
    store_insn(cpu, 0115, ElSvsAsm("зп 2010, зп 2011"));
    store_insn(cpu, 0116, ElSvsAsm("зп 2012, пб (15)"));
    store_insn(cpu, 0117, ElSvsAsm("сли 17(12), слиа 1(17)"));
    store_insn(cpu, 0120, ElSvsAsm("пино 101(17), слиа -1(2)"));
    store_insn(cpu, 0121, ElSvsAsm("пино 101(2), слиа 1(3)"));
    store_insn(cpu, 0122, ElSvsAsm("пино 101(3), пино 101(4)"));
    store_insn(cpu, 0123, ElSvsAsm("пино 101(5), слиа -1(6)"));
    store_insn(cpu, 0124, ElSvsAsm("пино 101(6), слиа 1(7)"));
    store_insn(cpu, 0125, ElSvsAsm("пино 101(7), пб (15)"));
    store_data(cpu, 02000, 07777777777777777ul);
    store_data(cpu, 02001, 05252525252525252ul);
    store_data(cpu, 02002, 02525252525252525ul);
    store_data(cpu, 02003, 00000000000000001ul);
    store_data(cpu, 02004, 00000000000000002ul);
    store_data(cpu, 02005, 00000000000000003ul);
    store_data(cpu, 02006, 00000000000000060ul);
    store_data(cpu, 02007, 07777777700000001ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 0100u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
    ct_assertequal(ElSvsGetM(cpu, 15), 02010u);
}

//
// Test: XTR, NTR, RТЕ, UZA, U1A, УТА instructions (РЖ, РЖА, СЧРЖ, ПО, ПЕ, СЧМР).
//
static void ntr_rte(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("уиа 2052(17), счи"));
    store_insn(cpu, 011, ElSvsAsm("уиа 77(2), уиа -77(3)"));
    store_insn(cpu, 012, ElSvsAsm("ржа (2), счрж 77"));
    store_insn(cpu, 013, ElSvsAsm("зп 2051, счим 2"));
    store_insn(cpu, 014, ElSvsAsm("сда 27, нтж (17)"));
    store_insn(cpu, 015, ElSvsAsm("пе 65, пе 65"));
    store_insn(cpu, 016, ElSvsAsm("рж, счрж 77"));
    store_insn(cpu, 017, ElSvsAsm("пе 65, рж 2051"));
    store_insn(cpu, 020, ElSvsAsm("счрж 77, счим 2"));
    store_insn(cpu, 021, ElSvsAsm("сда 27, нтж (17)"));
    store_insn(cpu, 022, ElSvsAsm("пе 65, пе 65"));
    store_insn(cpu, 023, ElSvsAsm("уиа 2052(17), рж (17)"));
    store_insn(cpu, 024, ElSvsAsm("уиа 2001(17), счрж 77"));
    store_insn(cpu, 025, ElSvsAsm("счим 2, сда 27"));
    store_insn(cpu, 026, ElSvsAsm("нтж (17), пе 65"));
    store_insn(cpu, 027, ElSvsAsm("слиа -1(2), цикл 12(3)"));
    store_insn(cpu, 030, ElSvsAsm("ржа 77, счрж 41"));
    store_insn(cpu, 031, ElSvsAsm("нтж 4057, пе 65"));
    store_insn(cpu, 032, ElSvsAsm("ржа, по 65"));
    store_insn(cpu, 033, ElSvsAsm("пе 34, пб 65"));
    store_insn(cpu, 034, ElSvsAsm("ржа 7, пе 65"));
    store_insn(cpu, 035, ElSvsAsm("ржа 13, по 65"));
    store_insn(cpu, 036, ElSvsAsm("или, пе 65"));
    store_insn(cpu, 037, ElSvsAsm("ржа 23, пе 65"));
    store_insn(cpu, 040, ElSvsAsm("сч 2000, по 65"));
    store_insn(cpu, 041, ElSvsAsm("ржа 13, пе 65"));
    store_insn(cpu, 042, ElSvsAsm("ржа 23, по 65"));
    store_insn(cpu, 043, ElSvsAsm("ржа 30, по 65"));
    store_insn(cpu, 044, ElSvsAsm("ржа 14, пе 65"));
    store_insn(cpu, 045, ElSvsAsm("сч 4060, ржа 24"));
    store_insn(cpu, 046, ElSvsAsm("пе 65, сч 2000"));
    store_insn(cpu, 047, ElSvsAsm("нтж, сч"));
    store_insn(cpu, 050, ElSvsAsm("счмр, нтж 2000"));
    store_insn(cpu, 051, ElSvsAsm("пе 65, слц"));
    store_insn(cpu, 052, ElSvsAsm("по 65, слц 2000"));
    store_insn(cpu, 053, ElSvsAsm("пе 65, и 2000"));
    store_insn(cpu, 054, ElSvsAsm("по 65, мода"));
    store_insn(cpu, 055, ElSvsAsm("сч, ржа 77"));
    store_insn(cpu, 056, ElSvsAsm("зп 2051, счрж 77"));
    store_insn(cpu, 057, ElSvsAsm("нтж 4061, пе 65"));
    store_insn(cpu, 060, ElSvsAsm("сч 2000, ржа"));
    store_insn(cpu, 061, ElSvsAsm("сч, по 63"));
    store_insn(cpu, 062, ElSvsAsm("пб 65, мода"));
    store_insn(cpu, 063, ElSvsAsm("ржа, сч"));
    store_insn(cpu, 064, ElSvsAsm("по 66, мода"));
    store_insn(cpu, 065, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_insn(cpu, 066, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_data(cpu, 02000, 07777777777777777ul);
    store_data(cpu, 04057, 02040000000000000ul);
    store_data(cpu, 04060, 00000000000000001ul);
    store_data(cpu, 04061, 03740000000000000ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 066u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
    ct_assertequal(ElSvsGetRAU(cpu), 04u);
    ct_assertequal(ElSvsGetM(cpu, 2), 077777u);
    ct_assertequal(ElSvsGetM(cpu, 3), 0u);
    ct_assertequal(ElSvsGetM(cpu, 15), 02001u);
}

//
// Test: YTA instruction (СЧМР).
//
static void yta(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("сч 2000, сда 160"));
    store_insn(cpu, 011, ElSvsAsm("счмр, зп 2002"));
    store_insn(cpu, 012, ElSvsAsm("счмр, нтж 2000"));
    store_insn(cpu, 013, ElSvsAsm("пе 70, сч 2002"));
    store_insn(cpu, 014, ElSvsAsm("нтж 2000, пе 70"));
    store_insn(cpu, 015, ElSvsAsm("сч 2000, сда 160"));
    store_insn(cpu, 016, ElSvsAsm("ржа 23, счмр 123"));
    store_insn(cpu, 017, ElSvsAsm("зп 2002, счмр 65"));
    store_insn(cpu, 020, ElSvsAsm("нтж 2003, пе 70"));
    store_insn(cpu, 021, ElSvsAsm("сч 2002, нтж 2004"));
    store_insn(cpu, 022, ElSvsAsm("пе 70, сч 2000"));
    store_insn(cpu, 023, ElSvsAsm("нтж 2001, сда 160"));
    store_insn(cpu, 024, ElSvsAsm("ржа 13, счмр 123"));
    store_insn(cpu, 025, ElSvsAsm("зп 2002, счмр 65"));
    store_insn(cpu, 026, ElSvsAsm("нтж 2005, пе 70"));
    store_insn(cpu, 027, ElSvsAsm("сч 2002, нтж 2006"));
    store_insn(cpu, 030, ElSvsAsm("пе 70, сч 2000"));
    store_insn(cpu, 031, ElSvsAsm("сда 160, ржа 3"));
    store_insn(cpu, 032, ElSvsAsm("счмр 123, зп 2002"));
    store_insn(cpu, 033, ElSvsAsm("счмр 65, нтж 2003"));
    store_insn(cpu, 034, ElSvsAsm("пе 70, сч 2002"));
    store_insn(cpu, 035, ElSvsAsm("нтж 2004, пе 70"));
    store_insn(cpu, 036, ElSvsAsm("сч 2000, сда 160"));
    store_insn(cpu, 037, ElSvsAsm("и 2001, счмр"));
    store_insn(cpu, 040, ElSvsAsm("пе 70, сч 2000"));
    store_insn(cpu, 041, ElSvsAsm("сда 160, или 2001"));
    store_insn(cpu, 042, ElSvsAsm("счмр, пе 70"));
    store_insn(cpu, 043, ElSvsAsm("сч 2000, сда 160"));
    store_insn(cpu, 044, ElSvsAsm("слц 2001, ржа 7"));
    store_insn(cpu, 045, ElSvsAsm("счмр, пе 70"));
    store_insn(cpu, 046, ElSvsAsm("сч 2000, сда 160"));
    store_insn(cpu, 047, ElSvsAsm("чед 2001, счмр"));
    store_insn(cpu, 050, ElSvsAsm("пе 70, сч 2000"));
    store_insn(cpu, 051, ElSvsAsm("сда 160, сбр 2001"));
    store_insn(cpu, 052, ElSvsAsm("счмр, пе 70"));
    store_insn(cpu, 053, ElSvsAsm("сч 2000, сда 160"));
    store_insn(cpu, 054, ElSvsAsm("рзб 2001, счмр"));
    store_insn(cpu, 055, ElSvsAsm("пе 70, сч 2000"));
    store_insn(cpu, 056, ElSvsAsm("по 70, счмр"));
    store_insn(cpu, 057, ElSvsAsm("нтж 2000, пе 70"));
    store_insn(cpu, 060, ElSvsAsm("и, сч 2000"));
    store_insn(cpu, 061, ElSvsAsm("пе 62, пб 70"));
    store_insn(cpu, 062, ElSvsAsm("счмр, нтж 2000"));
    store_insn(cpu, 063, ElSvsAsm("пе 70, и"));
    store_insn(cpu, 064, ElSvsAsm("сч 2000, нтж"));
    store_insn(cpu, 065, ElSvsAsm("счмр, нтж 2000"));
    store_insn(cpu, 066, ElSvsAsm("пе 70, мода"));
    store_insn(cpu, 067, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 070, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02000, 01234567123456712ul);
    store_data(cpu, 02001, 07777777777777777ul);
    store_data(cpu, 02002, 0ul);
    store_data(cpu, 02003, 00414567123456712ul);
    store_data(cpu, 02004, 01154567123456712ul);
    store_data(cpu, 02005, 00403210654321065ul);
    store_data(cpu, 02006, 01143210654321065ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 067u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
}

//
// Test: Е+N, Е-N, Е+Х, Е-Х instructions (СЛПА, ВЧПА, СЛП, ВЧП).
//
static void ean_esn_eax_esx(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("уиа 77602(14), сч 2000"));
    store_insn(cpu, 011, ElSvsAsm("зп 2003, мода"));
    store_insn(cpu, 012, ElSvsAsm("сч 2003, слпа 77"));
    store_insn(cpu, 013, ElSvsAsm("зп 2003, сда 151"));
    store_insn(cpu, 014, ElSvsAsm("уи 16, сли 16(14)"));
    store_insn(cpu, 015, ElSvsAsm("пино 34(16), цикл 12(14)"));
    store_insn(cpu, 016, ElSvsAsm("сч 2003, вчпа 101"));
    store_insn(cpu, 017, ElSvsAsm("по 34, уиа 77602(14)"));
    store_insn(cpu, 020, ElSvsAsm("сч 2001, зп 2003"));
    store_insn(cpu, 021, ElSvsAsm("уиа -1(13), мода"));
    store_insn(cpu, 022, ElSvsAsm("сч 2003, вчп 2002"));
    store_insn(cpu, 023, ElSvsAsm("зп 2003, сда 151"));
    store_insn(cpu, 024, ElSvsAsm("уи 16, сли 16(13)"));
    store_insn(cpu, 025, ElSvsAsm("пино 34(16), слиа -1(13)"));
    store_insn(cpu, 026, ElSvsAsm("цикл 22(14), сч 2003"));
    store_insn(cpu, 027, ElSvsAsm("слп 2001, нтж 2002"));
    store_insn(cpu, 030, ElSvsAsm("пе 34, сч 2004"));
    store_insn(cpu, 031, ElSvsAsm("слп 2005, нтж 2006"));
    store_insn(cpu, 032, ElSvsAsm("пе 34, мода"));
    store_insn(cpu, 033, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 034, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02000, 07750000000000000ul);
    store_data(cpu, 02001, 00010000000000000ul);
    store_data(cpu, 02002, 03750000000000000ul);
    store_data(cpu, 02003, 0ul);
    store_data(cpu, 02004, 07030000000000000ul);
    store_data(cpu, 02005, 04010000000000000ul);
    store_data(cpu, 02006, 06760000000000000ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 033u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
    ct_assertequal(ElSvsGetRAU(cpu), 04u);
    ct_assertequal(ElSvsGetM(cpu, 11), 077600u);
    ct_assertequal(ElSvsGetM(cpu, 14), 0u);
}

//
// Test: А+Х, А-Х, Х-А instructions (СЛ, ВЧ, ВЧОБ).
//
static void aax_asx_xsa(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm(" уиа 2000(17), ржа 3"));
    store_insn(cpu, 011, ElSvsAsm(" уиа 100(16), счи 16"));
    store_insn(cpu, 012, ElSvsAsm(" вч 2012, по 57"));
    store_insn(cpu, 013, ElSvsAsm(" сл 2013, пе 57"));
    store_insn(cpu, 014, ElSvsAsm(" или, пе 57"));
    store_insn(cpu, 015, ElSvsAsm(" сч 2014, вчоб 2013"));
    store_insn(cpu, 016, ElSvsAsm(" вч 2015, пе 57"));
    store_insn(cpu, 017, ElSvsAsm(" или, пе 57"));
    store_insn(cpu, 020, ElSvsAsm(" сч 2014, счм 2013"));
    store_insn(cpu, 021, ElSvsAsm(" счм 2014, счм 2016"));
    store_insn(cpu, 022, ElSvsAsm(" вч (17), пе 57"));
    store_insn(cpu, 023, ElSvsAsm(" сл (17), вчоб (17)"));
    store_insn(cpu, 024, ElSvsAsm(" пе 57, или"));
    store_insn(cpu, 025, ElSvsAsm(" пе 57, сч 2017"));
    store_insn(cpu, 026, ElSvsAsm(" вч 2020, по 57"));
    store_insn(cpu, 027, ElSvsAsm(" сл 2021, пе 57"));
    store_insn(cpu, 030, ElSvsAsm(" или, по 57"));
    store_insn(cpu, 031, ElSvsAsm(" нтж 2022, пе 57"));
    store_insn(cpu, 032, ElSvsAsm(" сч 2023, вч 2024"));
    store_insn(cpu, 033, ElSvsAsm(" нтж 2025, пе 57"));
    store_insn(cpu, 034, ElSvsAsm(" сч 2024, вч 2023"));
    store_insn(cpu, 035, ElSvsAsm(" нтж 2026, пе 57"));
    store_insn(cpu, 036, ElSvsAsm(" ржа 2, сч 2021"));
    store_insn(cpu, 037, ElSvsAsm(" счм 2027, счм 2021"));
    store_insn(cpu, 040, ElSvsAsm(" счм 2027, сл (17)"));
    store_insn(cpu, 041, ElSvsAsm(" вч (17), вчоб (17)"));
    store_insn(cpu, 042, ElSvsAsm(" пе 57, ржа 2"));
    store_insn(cpu, 043, ElSvsAsm(" сч 2030, вч 2031"));
    store_insn(cpu, 044, ElSvsAsm(" пе 57, нтж 2027"));
    store_insn(cpu, 045, ElSvsAsm(" пе 57, ржа 77"));
    store_insn(cpu, 046, ElSvsAsm(" сч 2032, сл 2032"));
    store_insn(cpu, 047, ElSvsAsm(" ржа, нтж 2033"));
    store_insn(cpu, 050, ElSvsAsm(" пе 57, ржа"));
    store_insn(cpu, 051, ElSvsAsm(" сч 2034, сл 2035"));
    store_insn(cpu, 052, ElSvsAsm(" нтж 2036, пе 57"));
    store_insn(cpu, 053, ElSvsAsm(" сч 2032, вчоб 2037"));
    store_insn(cpu, 054, ElSvsAsm(" счмр 100, нтж 2040"));
    store_insn(cpu, 055, ElSvsAsm(" пе 57, мода"));
    store_insn(cpu, 056, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 057, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail

    store_data(cpu, 02012, 00000000000000101ul);
    store_data(cpu, 02013, 00000000000000001ul);
    store_data(cpu, 02014, 00000000000000002ul);
    store_data(cpu, 02015, 00037777777777777ul);
    store_data(cpu, 02016, 00000000000000003ul);
    store_data(cpu, 02017, 06400000000000100ul);
    store_data(cpu, 02020, 06400000000000102ul);
    store_data(cpu, 02021, 04110000000000000ul);
    store_data(cpu, 02022, 06400000000000000ul);
    store_data(cpu, 02023, 06420000000000000ul);
    store_data(cpu, 02024, 06420000000000001ul);
    store_data(cpu, 02025, 06437777777777777ul);
    store_data(cpu, 02026, 06400000000000001ul);
    store_data(cpu, 02027, 04114000000000000ul);
    store_data(cpu, 02030, 04050000000000000ul);
    store_data(cpu, 02031, 04060000000000000ul);
    store_data(cpu, 02032, 00010000000000000ul);
    store_data(cpu, 02033, 00050000000000000ul);
    store_data(cpu, 02034, 07700000000001000ul);
    store_data(cpu, 02035, 04000000000000001ul);
    store_data(cpu, 02036, 06010000000000001ul);
    store_data(cpu, 02037, 04010000000000000ul);
    store_data(cpu, 02040, 03757777777600000ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 056u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
    ct_assertequal(ElSvsGetRAU(cpu), 04u);
    ct_assertequal(ElSvsGetM(cpu, 15), 02000u);
}

//
// Test: AMX instruction (ВЧАБ).
//
static void amx(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("уиа 2001(17), ржа 3"));
    store_insn(cpu, 011, ElSvsAsm("уиа 100(16), счи 16"));
    store_insn(cpu, 012, ElSvsAsm("вчаб 2013, по 34"));
    store_insn(cpu, 013, ElSvsAsm("нтж 2000, пе 34"));
    store_insn(cpu, 014, ElSvsAsm("сч 2000, вчаб 2000"));
    store_insn(cpu, 015, ElSvsAsm("пе 34, или"));
    store_insn(cpu, 016, ElSvsAsm("пе 34, сч 2014"));
    store_insn(cpu, 017, ElSvsAsm("счм 2015, вчаб (17)"));
    store_insn(cpu, 020, ElSvsAsm("пе 34, нтж 2016"));
    store_insn(cpu, 021, ElSvsAsm("пе 34, сч 2017"));
    store_insn(cpu, 022, ElSvsAsm("вчаб 2016, нтж 2020"));
    store_insn(cpu, 023, ElSvsAsm("пе 34, сч 2021"));
    store_insn(cpu, 024, ElSvsAsm("счм 2022, вчаб (17)"));
    store_insn(cpu, 025, ElSvsAsm("нтж 2023, пе 34"));
    store_insn(cpu, 026, ElSvsAsm("ржа, сч 2024"));
    store_insn(cpu, 027, ElSvsAsm("счм 2025, вчаб (17)"));
    store_insn(cpu, 030, ElSvsAsm("нтж 2021, пе 34"));
    store_insn(cpu, 031, ElSvsAsm("сч 2026, вчаб 2027"));
    store_insn(cpu, 032, ElSvsAsm("нтж 2030, пе 34"));
    store_insn(cpu, 033, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 034, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02000, 00037777777777777ul);
    store_data(cpu, 02013, 00000000000000101ul);
    store_data(cpu, 02014, 00000000000000002ul);
    store_data(cpu, 02015, 00000000000000003ul);
    store_data(cpu, 02016, 00000000000000001ul);
    store_data(cpu, 02017, 00067777777777777ul);
    store_data(cpu, 02020, 00050000000000000ul);
    store_data(cpu, 02021, 04050000000000000ul);
    store_data(cpu, 02022, 06427777777777777ul);
    store_data(cpu, 02023, 06410000000000000ul);
    store_data(cpu, 02024, 06410000000000002ul);
    store_data(cpu, 02025, 06410000000000003ul);
    store_data(cpu, 02026, 04060000000000000ul);
    store_data(cpu, 02027, 04057777777777765ul);
    store_data(cpu, 02030, 01653000000000000ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 033u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
    ct_assertequal(ElSvsGetRAU(cpu), 04u);
    ct_assertequal(ElSvsGetM(cpu, 15), 02001u);
}

//
// Test: AVX instruction (ЗНАК).
//
static void avx(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("уиа 2002(17), ржа 3"));
    store_insn(cpu, 011, ElSvsAsm("уиа 100(16), счи 16"));
    store_insn(cpu, 012, ElSvsAsm("знак 2000, пе 45"));
    store_insn(cpu, 013, ElSvsAsm("нтж 2014, пе 45"));
    store_insn(cpu, 014, ElSvsAsm("счи 16, знак 2001"));
    store_insn(cpu, 015, ElSvsAsm("по 45, нтж 2015"));
    store_insn(cpu, 016, ElSvsAsm("пе 45, сч 2001"));
    store_insn(cpu, 017, ElSvsAsm("знак 2001, пе 45"));
    store_insn(cpu, 020, ElSvsAsm("нтж 2000, пе 45"));
    store_insn(cpu, 021, ElSvsAsm("сч 2000, знак 2001"));
    store_insn(cpu, 022, ElSvsAsm("по 45, нтж 2016"));
    store_insn(cpu, 023, ElSvsAsm("пе 45, сч 2017"));
    store_insn(cpu, 024, ElSvsAsm("счм 2020, знак (17)"));
    store_insn(cpu, 025, ElSvsAsm("пе 45, нтж 2021"));
    store_insn(cpu, 026, ElSvsAsm("пе 45, ржа"));
    store_insn(cpu, 027, ElSvsAsm("сч 2001, знак 2001"));
    store_insn(cpu, 030, ElSvsAsm("пе 45, нтж 2000"));
    store_insn(cpu, 031, ElSvsAsm("пе 45, сч 2000"));
    store_insn(cpu, 032, ElSvsAsm("знак 2001, по 45"));
    store_insn(cpu, 033, ElSvsAsm("нтж 2001, пе 45"));
    store_insn(cpu, 034, ElSvsAsm("сч 2022, знак 2001"));
    store_insn(cpu, 035, ElSvsAsm("по 45, нтж 2023"));
    store_insn(cpu, 036, ElSvsAsm("пе 45, сч 2024"));
    store_insn(cpu, 037, ElSvsAsm("знак 2001, пе 45"));
    store_insn(cpu, 040, ElSvsAsm("нтж, пе 45"));
    store_insn(cpu, 041, ElSvsAsm("сч 2025, знак 2001"));
    store_insn(cpu, 042, ElSvsAsm("пе 45, нтж 2026"));
    store_insn(cpu, 043, ElSvsAsm("пе 45, мода"));
    store_insn(cpu, 044, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 045, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02000, 04050000000000000ul);
    store_data(cpu, 02001, 04020000000000000ul);
    store_data(cpu, 02014, 00000000000000100ul);
    store_data(cpu, 02015, 00037777777777700ul);
    store_data(cpu, 02016, 04070000000000000ul);
    store_data(cpu, 02017, 04060000000000000ul);
    store_data(cpu, 02020, 04124000000000000ul);
    store_data(cpu, 02021, 04114000000000000ul);
    store_data(cpu, 02022, 07757777777777777ul);
    store_data(cpu, 02023, 07760000000000001ul);
    store_data(cpu, 02024, 00010000000000000ul);
    store_data(cpu, 02025, 00027777777777777ul);
    store_data(cpu, 02026, 00010000000000001ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 044u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
    ct_assertequal(ElSvsGetRAU(cpu), 04u);
    ct_assertequal(ElSvsGetM(cpu, 15), 02002u);
}

//
// Test: multiplication in ALU.
//
static void alu_mul(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Disable normalization
    ElSvsSetRAU(cpu, 3);

    ElSvsSetAcc(cpu, 04050000000000000);                    // 1/2 * 2^1
    svs_multiply(cpu, 04050000000000000);                   // 1/2 * 2^1
    ct_assertequal(ElSvsGetAcc(cpu), 04104000000000000u);   // 1/4 * 2^2
    ct_assertequal(ElSvsGetRMR(cpu), 0u);

    ElSvsSetAcc(cpu, 04050000000000000);                    // 1/2 * 2^1
    svs_multiply(cpu, 04020000000000000);                   // -1 * 2^0
    ct_assertequal(ElSvsGetAcc(cpu), 04070000000000000u);   // -1/2 * 2^1
    ct_assertequal(ElSvsGetRMR(cpu), 0u);

    ElSvsSetAcc(cpu, 04020000000000000);                    // -1 * 2^0
    svs_multiply(cpu, 04050000000000000);                   // 1/2 * 2^1
    ct_assertequal(ElSvsGetAcc(cpu), 04070000000000000u);   // -1/2 * 2^1
    ct_assertequal(ElSvsGetRMR(cpu), 0u);

    ElSvsSetAcc(cpu, 04020000000000000);                    // -1 * 2^0
    svs_multiply(cpu, 04020000000000000);                   // -1 * 2^0
    ct_assertequal(ElSvsGetAcc(cpu), 04050000000000000u);   // 1/2 * 2^1
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
}

//
// Test: division in ALU.
//
static void alu_div(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Disable normalization
    ElSvsSetRAU(cpu, 3);

    // 1.0 / 1.0 gives 1.0
    ElSvsSetAcc(cpu, 04050000000000000);                    // 1/2 * 2^1
    svs_divide(cpu, 04050000000000000);                     // 1/2 * 2^1
    ct_assertequal(ElSvsGetAcc(cpu), 04050000000000000u);   // 1/2 * 2^1
    ct_assertequal(ElSvsGetRMR(cpu), 0u);

    // -1.0 / -1.0 gives 1.0
    ElSvsSetAcc(cpu, 04020000000000000);                    // -1 * 2^0
    svs_divide(cpu, 04020000000000000);                     // -1 * 2^0
    ct_assertequal(ElSvsGetAcc(cpu), 04050000000000000u);   // 1/2 * 2^1
    ct_assertequal(ElSvsGetRMR(cpu), 0u);

    // 1.0 / -1.0 gives -1.0 denormalized
    ElSvsSetAcc(cpu, 04050000000000000);                    // 1/2 * 2^1
    svs_divide(cpu, 04020000000000000);                     // -1 * 2^0
    ct_assertequal(ElSvsGetAcc(cpu), 04070000000000000u);   // -1/2 * 2^1
    ct_assertequal(ElSvsGetRMR(cpu), 0u);

    // -1.0 / 1.0 gives -1.0
    ElSvsSetAcc(cpu, 04020000000000000);                    // -1 * 2^0
    svs_divide(cpu, 04050000000000000);                     // 1/2 * 2^1
    ct_assertequal(ElSvsGetAcc(cpu), 04020000000000000u);   // -1 * 2^0
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
}

//
// Test: A*X instruction (УМН).
//
static void multiply(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("уиа 2001(17), ржа 3"));
    store_insn(cpu, 011, ElSvsAsm("сч 2013, умн 2014"));
    store_insn(cpu, 012, ElSvsAsm("зп (17), счмр 100"));
    store_insn(cpu, 013, ElSvsAsm("зпм 2000, нтж 2015"));
    store_insn(cpu, 014, ElSvsAsm("пе 37, сч 2000"));
    store_insn(cpu, 015, ElSvsAsm("нтж 2016, пе 37"));
    store_insn(cpu, 016, ElSvsAsm("сч 2017, умн 2020"));
    store_insn(cpu, 017, ElSvsAsm("зп (17), счмр 100"));
    store_insn(cpu, 020, ElSvsAsm("зпм 2000, нтж 2021"));
    store_insn(cpu, 021, ElSvsAsm("пе 37, сч 2000"));
    store_insn(cpu, 022, ElSvsAsm("слпа 130, нтж 2022"));
    store_insn(cpu, 023, ElSvsAsm("пе 37, ржа"));
    store_insn(cpu, 024, ElSvsAsm("сч 2023, умн 2024"));
    store_insn(cpu, 025, ElSvsAsm("нтж 2024, пе 37"));
    store_insn(cpu, 026, ElSvsAsm("сч 2024, умн 2023"));
    store_insn(cpu, 027, ElSvsAsm("нтж 2024, пе 37"));
    store_insn(cpu, 030, ElSvsAsm("сч 2024, умн 2024"));
    store_insn(cpu, 031, ElSvsAsm("нтж 2023, пе 37"));
    store_insn(cpu, 032, ElSvsAsm("ржа 2, сч 2025"));
    store_insn(cpu, 033, ElSvsAsm("умн 2026, зп 2000"));
    store_insn(cpu, 034, ElSvsAsm("нтж 2026, нтж 2027"));
    store_insn(cpu, 035, ElSvsAsm("пе 37, мода"));
    store_insn(cpu, 036, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 037, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02013, 06400000000000005);
    store_data(cpu, 02014, 02400000000000015);
    store_data(cpu, 02015, 05000000000000000);
    store_data(cpu, 02016, 05000000000000101);
    store_data(cpu, 02017, 02400000000000005);
    store_data(cpu, 02020, 06437777777777763);
    store_data(cpu, 02021, 05037777777777777);
    store_data(cpu, 02022, 06417777777777677);
    store_data(cpu, 02023, 04050000000000000);
    store_data(cpu, 02024, 04020000000000000);
    store_data(cpu, 02025, 04110000000000000);
    store_data(cpu, 02026, 04114000000000000);
    store_data(cpu, 02027, 00040000000000000);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 036u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
    ct_assertequal(ElSvsGetRAU(cpu), 06u);
    ct_assertequal(ElSvsGetM(cpu, 15), 02001u);
}

//
// Test: A/X instruction (ДЕЛ).
//
static void divide(void *context)
{
    struct ElSvsProcessor *cpu = context;

    // Store the test code.
    store_insn(cpu, 010, ElSvsAsm("уиа 2000(17), ржа 3"));
    store_insn(cpu, 011, ElSvsAsm("сч 2012, дел 2013"));
    store_insn(cpu, 012, ElSvsAsm("нтж 2014, пе 14"));
    store_insn(cpu, 013, ElSvsAsm("стоп 12345(6), мода")); // Magic opcode: Pass
    store_insn(cpu, 014, ElSvsAsm("стоп 76543(2), мода")); // Magic opcode: Fail
    store_data(cpu, 02012, 04154000000000000ul);
    store_data(cpu, 02013, 04114000000000000ul);
    store_data(cpu, 02014, 04110000000000000ul);

    // Run the code.
    ElSvsSetPC(cpu, 010);
    int status = ElSvsSimulate(cpu);
    ct_assertequal(status, ESS_HALT);

    // Check registers.
    ct_assertequal(ElSvsGetPC(cpu), 013u);
    ct_assertequal(ElSvsGetAcc(cpu), 0u);
    ct_assertequal(ElSvsGetRMR(cpu), 0u);
    ct_assertequal(ElSvsGetRAU(cpu), 07u);
    ct_assertequal(ElSvsGetM(cpu, 15), 02000u);
}

//
// Run all tests.
//
int main(int argc, char *argv[])
{
    // List of all tests.
    const struct ct_testcase tests[] = {
        ct_maketest(uj),
        ct_maketest(vtm_vzm_v1m),
        ct_maketest(jam_utm),
        ct_maketest(vlm),
        ct_maketest(utc_wtc),
        ct_maketest(vjm),
        ct_maketest(mtj),
        ct_maketest(xta_uza_u1a),
        ct_maketest(atx),
        ct_maketest(ati_ita),
        ct_maketest(addr0),
        ct_maketest(aax_aox_aex),
        ct_maketest(arx),
        ct_maketest(its),
        ct_maketest(sti),
        ct_maketest(xts),
        ct_maketest(stx),
        ct_maketest(asn_asx),
        ct_maketest(acx_anx),
        ct_maketest(apx_aux),
        ct_maketest(stack),
        ct_maketest(ntr_rte),
        ct_maketest(yta),
        ct_maketest(ean_esn_eax_esx),
        ct_maketest(aax_asx_xsa),
        ct_maketest(amx),
        ct_maketest(avx),
        ct_maketest(alu_mul),
        ct_maketest(alu_div),
        ct_maketest(multiply),
        ct_maketest(divide),
    };
    const struct ct_testsuite suite = ct_makesuite_setup_teardown(tests, setup, teardown);

    // Enable stdout/stderr from the tests.
    setenv("CINYTEST_SUPPRESS_OUTPUT", "no", 0);

    unlink(log_filename);
    if (ct_runsuite_withargs(&suite, argc, argv) != 0) {
        // Some tests failed.
        return 1;
    }

    // All tests passed.
    return 0;
}
