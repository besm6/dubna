//
// BESM-6 processor.
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
#include "machine.h"

//
// Initialize the processor.
//
Processor::Processor(Machine &machine, Memory &memory) :
    machine(machine),
    memory(memory)
{
    reset();
}

//
// Reset routine
//
void Processor::reset()
{
    core = {};
    core.PC = 1;

    //TODO: machine.trace_cpu_reset();
    //if (trace_instructions | trace_extracodes | trace_fetch |
    //    trace_memory | trace_exceptions | trace_registers) {
    //    fprintf(log_output, "cpu --- Reset\n");
    //}
}

//
// Main instruction fetch/decode loop.
//
void Processor::run()
{
    // TODO: Trace initial state.
    //if (trace_registers) {
    //    machine.trace_registers(*this);
    //}

    // Mask PC.
    core.PC &= BITS(15);

    // An internal interrupt or user intervention
    int exception_status = setjmp(exception);
    if (exception_status) {
        //TODO: machine.trace_exception();
        //if (trace_instructions | trace_memory |
        //    trace_registers | trace_fetch) {
        //    fprintf(log_output, "cpu --- exception\n");
        //}

        core.M[017] += corr_stack;
        return;
    }

    for (;;) {
        step();
    }
}

//
// Execute one instruction, placed at address PC+right_instr_flag.
// When stopped, perform a longjmp to exception.
//
void Processor::step()
{
    int reg, opcode, addr, nextpc, next_mod;
    uint64_t word;

    corr_stack = 0;
    word = mem_fetch(core.PC);
    if (core.right_instr_flag)
        RK = (unsigned)word;         // get right instruction
    else
        RK = (unsigned)(word >> 24); // get left instruction

    RK &= BITS(24);

    reg = RK >> 20;
    if (RK & BBIT(20)) {
        addr = RK & BITS(15);
        opcode = (RK >> 12) & 0370;
    } else {
        addr = RK & BITS(12);
        if (RK & BBIT(19))
            addr |= 070000;
        opcode = (RK >> 12) & 077;
    }

    // TODO: Трассировка команды: адрес, код и мнемоника.
    //if (trace_instructions ||
    //    (trace_extracodes && is_extracode(opcode))) {
    //    machine.trace_opcode(core.PC);
    //}

    nextpc = ADDR(core.PC + 1);
    if (core.right_instr_flag) {
        core.PC += 1;                               // increment PC
        core.right_instr_flag = false;
    } else {
        core.right_instr_flag = true;
    }

    if (core.apply_mod_reg) {
        addr = ADDR(addr + core.M[020]);
    }
    next_mod = 0;

    switch (opcode) {
    case 000:                                       // зп, atx
        Aex = ADDR(addr + core.M[reg]);
        mem_store(Aex, core.ACC);
        if (! addr && reg == 017)
            core.M[017] = ADDR(core.M[017] + 1);
        break;
    case 001:                                       // зпм, stx
        Aex = ADDR(addr + core.M[reg]);
        mem_store(Aex, core.ACC);
        core.M[017] = ADDR(core.M[017] - 1);
        corr_stack = 1;
        core.ACC = mem_load(core.M[017]);
        core.RAU = SET_LOGICAL(core.RAU);
        break;
    case 002:                                       // рег, mod
        longjmp(exception, ESS_BADCMD);
        break;
    case 003:                                       // счм, xts
        mem_store(core.M[017], core.ACC);
        core.M[017] = ADDR(core.M[017] + 1);
        corr_stack = -1;
        Aex = ADDR(addr + core.M[reg]);
        core.ACC = mem_load(Aex);
        core.RAU = SET_LOGICAL(core.RAU);
        break;
    case 004:                                       // сл, a+x
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_add(mem_load(Aex), 0, 0);
        core.RAU = SET_ADDITIVE(core.RAU);
        break;
    case 005:                                       // вч, a-x
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_add(mem_load(Aex), 0, 1);
        core.RAU = SET_ADDITIVE(core.RAU);
        break;
    case 006:                                       // вчоб, x-a
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_add(mem_load(Aex), 1, 0);
        core.RAU = SET_ADDITIVE(core.RAU);
        break;
    case 007:                                       // вчаб, amx
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_add(mem_load(Aex), 1, 1);
        core.RAU = SET_ADDITIVE(core.RAU);
        break;
    case 010:                                       // сч, xta
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        core.ACC = mem_load(Aex);
        core.RAU = SET_LOGICAL(core.RAU);
        break;
    case 011:                                       // и, aax
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        core.ACC &= mem_load(Aex);
        core.RMR = 0;
        core.RAU = SET_LOGICAL(core.RAU);
        break;
    case 012:                                       // нтж, aex
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        core.RMR = core.ACC;
        core.ACC ^= mem_load(Aex);
        core.RAU = SET_LOGICAL(core.RAU);
        break;
    case 013:                                       // слц, arx
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        core.ACC += mem_load(Aex);
        if (core.ACC & BIT49)
            core.ACC = (core.ACC + 1) & BITS48;
        core.RMR = 0;
        core.RAU = SET_MULTIPLICATIVE(core.RAU);
        break;
    case 014:                                       // знак, avx
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_change_sign(mem_load(Aex) >> 40 & 1);
        core.RAU = SET_ADDITIVE(core.RAU);
        break;
    case 015:                                       // или, aox
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        core.ACC |= mem_load(Aex);
        core.RMR = 0;
        core.RAU = SET_LOGICAL(core.RAU);
        break;
    case 016:                                       // дел, a/x
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_divide(mem_load(Aex));
        core.RAU = SET_MULTIPLICATIVE(core.RAU);
        break;
    case 017:                                       // умн, a*x
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_multiply(mem_load(Aex));
        core.RAU = SET_MULTIPLICATIVE(core.RAU);
        break;
    case 020:                                       // сбр, apx
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        core.ACC = besm6_pack(core.ACC, mem_load(Aex));
        core.RMR = 0;
        core.RAU = SET_LOGICAL(core.RAU);
        break;
    case 021:                                       // рзб, aux
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        core.ACC = besm6_unpack(core.ACC, mem_load(Aex));
        core.RMR = 0;
        core.RAU = SET_LOGICAL(core.RAU);
        break;
    case 022:                                       // чед, acx
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        core.ACC = besm6_count_ones(core.ACC) + mem_load(Aex);
        if (core.ACC & BIT49)
            core.ACC = (core.ACC + 1) & BITS48;
        core.RMR = 0;
        core.RAU = SET_LOGICAL(core.RAU);
        break;
    case 023:                                       // нед, anx
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        if (core.ACC) {
            int n = besm6_highest_bit(core.ACC);

            // "Остаток" сумматора, исключая бит,
            // номер которого определен, помещается в РМР,
            // начиная со старшего бита РМР.
            arith_shift(48 - n);

            // Циклическое сложение номера со словом по Аисп.
            core.ACC = n + mem_load(Aex);
            if (core.ACC & BIT49)
                core.ACC = (core.ACC + 1) & BITS48;
        } else {
            core.RMR = 0;
            core.ACC = mem_load(Aex);
        }
        core.RAU = SET_LOGICAL(core.RAU);
        break;
    case 024:                                       // слп, e+x
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_add_exponent((mem_load(Aex) >> 41) - 64);
        core.RAU = SET_MULTIPLICATIVE(core.RAU);
        break;
    case 025:                                       // вчп, e-x
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_add_exponent(64 - (mem_load(Aex) >> 41));
        core.RAU = SET_MULTIPLICATIVE(core.RAU);
        break;
    case 026: {                                     // сд, asx
        int n;
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        n = (mem_load(Aex) >> 41) - 64;
        arith_shift(n);
        core.RAU = SET_LOGICAL(core.RAU);
        break;
    }
    case 027:                                       // рж, xtr
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        core.RAU = (mem_load(Aex) >> 41) & 077;
        break;
    case 030:                                       // счрж, rte
        Aex = ADDR(addr + core.M[reg]);
        core.ACC = (uint64_t) (core.RAU & Aex & 0177) << 41;
        core.RAU = SET_LOGICAL(core.RAU);
        break;
    case 031:                                       // счмр, yta
        Aex = ADDR(addr + core.M[reg]);
        if (IS_LOGICAL(core.RAU)) {
            core.ACC = core.RMR;
        } else {
            uint64_t x = core.RMR;
            core.ACC = (core.ACC & ~BITS41) | (core.RMR & BITS40);
            arith_add_exponent((Aex & 0177) - 64);
            core.RMR = x;
        }
        break;
    case 032:                                       // зпп, запись полноразрядная
        longjmp(exception, ESS_BADCMD);
        break;
    case 033:                                       // счп, считывание полноразрядное
        longjmp(exception, ESS_BADCMD);
        break;
    case 034:                                       // слпа, e+n
        Aex = ADDR(addr + core.M[reg]);
        arith_add_exponent((Aex & 0177) - 64);
        core.RAU = SET_MULTIPLICATIVE(core.RAU);
        break;
    case 035:                                       // вчпа, e-n
        Aex = ADDR(addr + core.M[reg]);
        arith_add_exponent(64 - (Aex & 0177));
        core.RAU = SET_MULTIPLICATIVE(core.RAU);
        break;
    case 036: {                                     // сда, asn
        int n;
        Aex = ADDR(addr + core.M[reg]);
        n = (Aex & 0177) - 64;
        arith_shift(n);
        core.RAU = SET_LOGICAL(core.RAU);
        break;
    }
    case 037:                                       // ржа, ntr
        Aex = ADDR(addr + core.M[reg]);
        core.RAU = Aex & 077;
        break;
    case 040:                                       // уи, ati
        Aex = ADDR(addr + core.M[reg]);
        core.M[Aex & 017] = ADDR(core.ACC);
        core.M[0] = 0;
        break;
    case 041: {                                     // уим, sti
        unsigned rg, ad;

        Aex = ADDR(addr + core.M[reg]);
        rg = Aex & 017;
        ad = ADDR(core.ACC);
        if (rg != 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        core.ACC = mem_load(rg != 017 ? core.M[017] : ad);
        core.M[rg] = ad;
        core.M[0] = 0;
        core.RAU = SET_LOGICAL(core.RAU);
        break;
    }
    case 042:                                       // счи, ita
load_modifier:
        Aex = ADDR(addr + core.M[reg]);
        core.ACC = ADDR(core.M[Aex & 017]);
        core.RAU = SET_LOGICAL(core.RAU);
        break;
    case 043:                                       // счим, its
        mem_store(core.M[017], core.ACC);
        core.M[017] = ADDR(core.M[017] + 1);
        goto load_modifier;
    case 044:                                       // уии, mtj
        Aex = addr;
        core.M[Aex & 017] = core.M[reg];
        core.M[0] = 0;
        break;
    case 045:                                       // сли, j+m
        Aex = addr;
        core.M[Aex & 017] = ADDR(core.M[Aex & 017] + core.M[reg]);
        core.M[0] = 0;
        break;
    case 046:                                       // cоп, специальное обращение к памяти
        longjmp(exception, ESS_BADCMD);
        break;
    case 047:                                       // э47, x47
        longjmp(exception, ESS_BADCMD);
        break;
    case 050: case 051: case 052: case 053:
    case 054: case 055: case 056: case 057:
    case 060: case 061: case 062: case 063:
    case 064: case 065: case 066: case 067:
    case 070: case 071: case 072: case 073:
    case 074: case 075: case 076: case 077:         // э50...э77
    case 0200:                                      // э20
    case 0210:                                      // э21
       Aex = ADDR(addr + core.M[reg]);
       core.M[14] = Aex;
       core.right_instr_flag = false;
       extracode(opcode);
       break;
    case 0220:                                      // мода, utc
        Aex = ADDR(addr + core.M[reg]);
        next_mod = Aex;
        break;
    case 0230:                                      // мод, wtc
        if (! addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        next_mod = ADDR(mem_load(Aex));
        break;
    case 0240:                                      // уиа, vtm
        Aex = addr;
        core.M[reg] = addr;
        core.M[0] = 0;
        break;
    case 0250:                                      // слиа, utm
        Aex = ADDR(addr + core.M[reg]);
        core.M[reg] = Aex;
        core.M[0] = 0;
        break;
    case 0260:                                      // по, uza
        Aex = ADDR(addr + core.M[reg]);
        core.RMR = core.ACC;
        if (IS_ADDITIVE(core.RAU)) {
            if (core.ACC & BIT41)
                break;
        } else if (IS_MULTIPLICATIVE(core.RAU)) {
            if (! (core.ACC & BIT48))
                break;
        } else if (IS_LOGICAL(core.RAU)) {
            if (core.ACC)
                break;
        } else
            break;
        core.PC = Aex;
        core.right_instr_flag = false;
        break;
    case 0270:                                      // пе, u1a
        Aex = ADDR(addr + core.M[reg]);
        core.RMR = core.ACC;
        if (IS_ADDITIVE(core.RAU)) {
            if (! (core.ACC & BIT41))
                break;
        } else if (IS_MULTIPLICATIVE(core.RAU)) {
            if (core.ACC & BIT48)
                break;
        } else if (IS_LOGICAL(core.RAU)) {
            if (! core.ACC)
                break;
        } else {
            // fall thru, i.e. branch
        }
        core.PC = Aex;
        core.right_instr_flag = false;
        break;
    case 0300:                                      // пб, uj
        Aex = ADDR(addr + core.M[reg]);
        core.PC = Aex;
        core.right_instr_flag = false;
        break;
    case 0310:                                      // пв, vjm
        Aex = addr;
        core.M[reg] = nextpc;
        core.M[0] = 0;
        core.PC = addr;
        core.right_instr_flag = false;
        break;
    case 0320:                                      // выпр, iret
        longjmp(exception, ESS_BADCMD);
        break;
    case 0330:                                      // стоп, stop
        break;
    case 0340:                                      // пио, vzm
branch_zero:
        Aex = addr;
        if (! core.M[reg]) {
            core.PC = addr;
            core.right_instr_flag = false;
        }
        break;
    case 0350:                                      // пино, v1m
        Aex = addr;
        if (core.M[reg]) {
            core.PC = addr;
            core.right_instr_flag = false;
        }
        break;
    case 0360:                                      // э36, *36
        // Как ПИО, но с выталкиванием БРЗ.
        goto branch_zero;
    case 0370:                                      // цикл, vlm
        Aex = addr;
        if (! core.M[reg])
            break;
        core.M[reg] = ADDR(core.M[reg] + 1);
        core.PC = addr;
        core.right_instr_flag = false;
        break;
    default:
        // Unknown instruction - cannot happen.
        longjmp(exception, ESS_BADCMD);
        break;
    }

    if (next_mod != 0) {
        // Модификация адреса следующей команды.
        core.M[020] = next_mod;
        core.apply_mod_reg = true;
    } else {
        core.apply_mod_reg = false;
    }

    // TODO: Трассировка изменённых регистров.
    //if (trace_registers) {
    //    machine.trace_registers(cpu);
    //}
}

//
// Fetch instruction word.
//
uint64_t Processor::mem_fetch(unsigned addr)
{
    if (addr == 0) {
        //TODO: machine.trace_jump_to_zero();
        //if (trace_exceptions)
        //    printf("--- jump to zero");
        longjmp(exception, ESS_JUMPZERO);
    }

    uint64_t val = memory.load(addr);

    if (!core.right_instr_flag) {
        //TODO: machine.trace_fetch();
        //if (trace_fetch) {
        //    fprintf(log_output, "cpu       Fetch [%05o] = ", addr, val);
        //    besm6_fprint_insn(log_output, (val >> 24) & BITS(24));
        //    besm6_fprint_insn(log_output, val & BITS(24));
        //    fprintf(log_output, "\n");
        //}
    }
    return val & BITS48;
}

//
// Write word to memory.
//
void Processor::mem_store(unsigned addr, uint64_t val)
{
    addr &= BITS(15);
    if (addr == 0)
        return;

    memory.store(addr, val);

    if (addr != 0) {
        //TODO: machine.trace_store();
        //if (trace_memory) {
        //    fprintf(log_output, "cpu       Memory Write [%05o] = ", addr);
        //    besm6_fprint_48bits(log_output, val);
        //    fprintf(log_output, "\n");
        //}
    }
}

//
// Read word from memory.
//
uint64_t Processor::mem_load(unsigned addr)
{
    addr &= BITS(15);
    if (addr == 0)
        return 0;

    uint64_t val = memory.load(addr);

    if (addr != 0) {
        //TODO: machine.trace_load();
        //if (cpu->trace_memory) {
        //    fprintf(cpu->log_output, "cpu       Memory Read [%05o] = ", addr);
        //    besm6_fprint_48bits(cpu->log_output, val);
        //    fprintf(cpu->log_output, "\n");
        //}
    }
    return val & BITS48;
}
