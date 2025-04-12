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
Processor::Processor(Machine &mach, Memory &mem) : machine(mach), memory(mem)
{
    reset();
}

//
// Finish the processor
//
void Processor::finish()
{
    // Emit the final newline.
    e64_finish();
}

//
// Reset routine
//
void Processor::reset()
{
    core    = {};
    core.PC = 1;

    machine.trace_exception("Reset");
}

//
// Stack correction in case of exception.
//
void Processor::stack_correction()
{
    core.M[017] += corr_stack;
    corr_stack = 0;
}

//
// Intercept ofvl/divzero exception, when enabled.
// Return true when intercepted.
// Return false when interception is disabled.
//
bool Processor::intercept(const std::string &message)
{
    if (intercept_count > 0 &&            // interception enabled
        (message == MSG_ARITH_OVERFLOW || // arithmetic overflow
         message == MSG_ARITH_DIVZERO)) { // divide by zero

        intercept_count--;

        // Jump to dedicated address.
        core.PC               = intercept_addr;
        core.right_instr_flag = false;
        core.apply_mod_reg    = false;
        core.MOD              = 0;
        return true;
    } else {
        return false;
    }
}

//
// Execute one instruction, placed at address PC+right_instr_flag.
// Return false to continue the program.
// Return true when the program is done and the processor is stopped.
// Emit exception in case of failure.
//
bool Processor::step()
{
    unsigned reg, opcode, addr, nextpc, next_mod;
    Word word;

    corr_stack = 0;
    core.PC &= BITS(15);
    word = machine.mem_fetch(core.PC);
    if (core.right_instr_flag)
        RK = (unsigned)word; // get right instruction
    else
        RK = (unsigned)(word >> 24); // get left instruction

    RK &= BITS(24);

    reg = RK >> 20;
    if (RK & ONEBIT(20)) {
        addr   = RK & BITS(15);
        opcode = (RK >> 12) & 0370;
    } else {
        addr = RK & BITS(12);
        if (RK & ONEBIT(19))
            addr |= 070000;
        opcode = (RK >> 12) & 077;
    }

    // Show instruction: address, opcode and mnemonics.
    machine.trace_instruction(opcode);

    nextpc = ADDR(core.PC + 1);
    if (core.right_instr_flag) {
        core.PC += 1; // increment PC
        core.right_instr_flag = false;
    } else {
        core.right_instr_flag = true;
    }

    if (core.apply_mod_reg) {
        addr = ADDR(addr + core.MOD);
    }
    next_mod = 0;

    switch (opcode) {
    case 000: // зп, atx
        Aex = ADDR(addr + core.M[reg]);
        machine.mem_store(Aex, core.ACC);
        machine.trace_memory_write_dispak(Aex, core.ACC);
        if (!addr && reg == 017)
            core.M[017] = ADDR(core.M[017] + 1);
        break;

    case 001: // зпм, stx
        Aex = ADDR(addr + core.M[reg]);
        machine.mem_store(Aex, core.ACC);
        machine.trace_memory_write_dispak(Aex, core.ACC);
        core.M[017] = ADDR(core.M[017] - 1);
        corr_stack  = 1;
        core.ACC    = machine.mem_load(core.M[017]);
        core.set_logical();
        break;

    case 002: // рег, mod
        throw Exception("Illegal instruction 002 рег/mod");

    case 003: // счм, xts
        machine.mem_store(core.M[017], core.ACC);
        core.M[017] = ADDR(core.M[017] + 1);
        corr_stack  = -1;
        Aex         = ADDR(addr + core.M[reg]);
        core.ACC    = machine.mem_load(Aex);
        core.set_logical();
        break;

    case 004: // сл, a+x
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_add(machine.mem_load(Aex), 0, 0);
        core.set_additive();
        break;

    case 005: // вч, a-x
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_add(machine.mem_load(Aex), 0, 1);
        core.set_additive();
        break;

    case 006: // вчоб, x-a
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_add(machine.mem_load(Aex), 1, 0);
        core.set_additive();
        break;

    case 007: // вчаб, amx
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_add(machine.mem_load(Aex), 1, 1);
        core.set_additive();
        break;

    case 010: // сч, xta
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex      = ADDR(addr + core.M[reg]);
        core.ACC = machine.mem_load(Aex);
        core.set_logical();
        break;

    case 011: // и, aax
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        core.ACC &= machine.mem_load(Aex);
        core.RMR = 0;
        core.set_logical();
        break;

    case 012: // нтж, aex
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex      = ADDR(addr + core.M[reg]);
        core.RMR = core.ACC;
        core.ACC ^= machine.mem_load(Aex);
        core.set_logical();
        break;

    case 013: // слц, arx
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        core.ACC += machine.mem_load(Aex);
        if (core.ACC & ONEBIT(49))
            core.ACC = (core.ACC + 1) & BITS48;
        core.RMR = 0;
        core.set_multiplicative();
        break;

    case 014: // знак, avx
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_change_sign(machine.mem_load(Aex) >> 40 & 1);
        core.set_additive();
        break;

    case 015: // или, aox
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        core.ACC |= machine.mem_load(Aex);
        core.RMR = 0;
        core.set_logical();
        break;

    case 016: // дел, a/x
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_divide(machine.mem_load(Aex));
        core.set_multiplicative();
        break;

    case 017: // умн, a*x
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_multiply(machine.mem_load(Aex));
        core.set_multiplicative();
        break;

    case 020: // сбр, apx
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex      = ADDR(addr + core.M[reg]);
        core.ACC = besm6_pack(core.ACC, machine.mem_load(Aex));
        core.RMR = 0;
        core.set_logical();
        break;

    case 021: // рзб, aux
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex      = ADDR(addr + core.M[reg]);
        core.ACC = besm6_unpack(core.ACC, machine.mem_load(Aex));
        core.RMR = 0;
        core.set_logical();
        break;

    case 022: // чед, acx
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex      = ADDR(addr + core.M[reg]);
        core.ACC = besm6_count_ones(core.ACC) + machine.mem_load(Aex);
        if (core.ACC & ONEBIT(49))
            core.ACC = (core.ACC + 1) & BITS48;
        core.RMR = 0;
        core.set_logical();
        break;

    case 023: // нед, anx
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        if (core.ACC) {
            int n = besm6_highest_bit(core.ACC);

            // "Остаток" сумматора, исключая бит,
            // номер которого определен, помещается в РМР,
            // начиная со старшего бита РМР.
            arith_shift(48 - n);

            // Циклическое сложение номера со словом по Аисп.
            core.ACC = n + machine.mem_load(Aex);
            if (core.ACC & ONEBIT(49))
                core.ACC = (core.ACC + 1) & BITS48;
        } else {
            core.RMR = 0;
            core.ACC = machine.mem_load(Aex);
        }
        core.set_logical();
        break;

    case 024: // слп, e+x
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_add_exponent((machine.mem_load(Aex) >> 41) - 64);
        core.set_multiplicative();
        break;

    case 025: // вчп, e-x
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        arith_add_exponent(64 - (machine.mem_load(Aex) >> 41));
        core.set_multiplicative();
        break;

    case 026: { // сд, asx
        int n;
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex = ADDR(addr + core.M[reg]);
        n   = (machine.mem_load(Aex) >> 41) - 64;
        arith_shift(n);
        core.set_logical();
        break;
    }

    case 027: // рж, xtr
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex      = ADDR(addr + core.M[reg]);
        core.RAU = (machine.mem_load(Aex) >> 41) & 077;
        break;

    case 030: // счрж, rte
        Aex      = ADDR(addr + core.M[reg]);
        core.ACC = (Word)(core.RAU & Aex & 0177) << 41;
        core.set_logical();
        break;

    case 031: // счмр, yta
        Aex = ADDR(addr + core.M[reg]);
        if (core.is_logical()) {
            core.ACC = core.RMR;
        } else {
            Word x   = core.RMR;
            core.ACC = (core.ACC & ~BITS41) | (core.RMR & BITS40);
            arith_add_exponent((Aex & 0177) - 64);
            core.RMR = x;
        }
        break;

    case 032: // зпп, запись полноразрядная
        throw Exception("Illegal instruction 032 зпп");

    case 033: // счп, считывание полноразрядное
        throw Exception("Illegal instruction 033 счп");

    case 034: // слпа, e+n
        Aex = ADDR(addr + core.M[reg]);
        arith_add_exponent((Aex & 0177) - 64);
        core.set_multiplicative();
        break;

    case 035: // вчпа, e-n
        Aex = ADDR(addr + core.M[reg]);
        arith_add_exponent(64 - (Aex & 0177));
        core.set_multiplicative();
        break;

    case 036: { // сда, asn
        int n;
        Aex = ADDR(addr + core.M[reg]);
        n   = (Aex & 0177) - 64;
        arith_shift(n);
        core.set_logical();
        break;
    }

    case 037: // ржа, ntr
        Aex      = ADDR(addr + core.M[reg]);
        core.RAU = Aex & 077;
        break;

    case 040: // уи, ati
        Aex               = ADDR(addr + core.M[reg]);
        core.M[Aex & 017] = ADDR(core.ACC);
        core.M[0]         = 0;
        break;

    case 041: { // уим, sti
        unsigned rg, ad;

        Aex = ADDR(addr + core.M[reg]);
        rg  = Aex & 017;
        ad  = ADDR(core.ACC);
        if (rg != 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        core.ACC   = machine.mem_load(rg != 017 ? core.M[017] : ad);
        core.M[rg] = ad;
        core.M[0]  = 0;
        core.set_logical();
        break;
    }

    case 042: // счи, ita
    load_modifier:
        Aex      = ADDR(addr + core.M[reg]);
        core.ACC = ADDR(core.M[Aex & 017]);
        core.set_logical();
        break;

    case 043: // счим, its
        machine.mem_store(core.M[017], core.ACC);
        core.M[017] = ADDR(core.M[017] + 1);
        goto load_modifier;

    case 044: // уии, mtj
        Aex               = addr;
        core.M[Aex & 017] = core.M[reg];
        core.M[0]         = 0;
        break;

    case 045: // сли, j+m
        Aex               = addr;
        core.M[Aex & 017] = ADDR(core.M[Aex & 017] + core.M[reg]);
        core.M[0]         = 0;
        break;

    case 046: // cоп, специальное обращение к памяти
        throw Exception("Illegal instruction 046 cоп");

    case 047: // э47, x47
        throw Exception("Illegal instruction 047");

    case 050:
    case 051:
    case 052:
    case 053:
    case 054:
    case 055:
    case 056:
    case 057:
    case 060:
    case 061:
    case 062:
    case 063:
    case 064:
    case 065:
    case 066:
    case 067:
    case 070:
    case 071:
    case 072:
    case 073:
    case 074:
    case 075:
    case 076:
    case 077:  // э50...э77
    case 0200: // э20
    case 0210: // э21
        Aex        = ADDR(addr + core.M[reg]);
        core.M[14] = Aex;
        extracode(opcode);
        core.set_logical();
        break;

    case 0220: // мода, utc
        Aex      = ADDR(addr + core.M[reg]);
        next_mod = Aex;
        break;

    case 0230: // мод, wtc
        if (!addr && reg == 017) {
            core.M[017] = ADDR(core.M[017] - 1);
            corr_stack  = 1;
        }
        Aex      = ADDR(addr + core.M[reg]);
        next_mod = ADDR(machine.mem_load(Aex));
        break;

    case 0240: // уиа, vtm
        Aex         = addr;
        core.M[reg] = addr;
        core.M[0]   = 0;
        if (reg == 0) {
            // VTM instruction with register 0: enable and disable tracing.
            Machine::enable_trace(addr);
        }
        break;

    case 0250: // слиа, utm
        Aex         = ADDR(addr + core.M[reg]);
        core.M[reg] = Aex;
        core.M[0]   = 0;
        break;

    case 0260: // по, uza
        Aex      = ADDR(addr + core.M[reg]);
        core.RMR = core.ACC;
        if (core.is_additive()) {
            if (core.ACC & BIT41)
                break;
        } else if (core.is_multiplicative()) {
            if (!(core.ACC & BIT48))
                break;
        } else if (core.is_logical()) {
            if (core.ACC)
                break;
        } else
            break;
        core.PC               = Aex;
        core.right_instr_flag = false;
        break;

    case 0270: // пе, u1a
        Aex      = ADDR(addr + core.M[reg]);
        core.RMR = core.ACC;
        if (core.is_additive()) {
            if (!(core.ACC & BIT41))
                break;
        } else if (core.is_multiplicative()) {
            if (core.ACC & BIT48)
                break;
        } else if (core.is_logical()) {
            if (!core.ACC)
                break;
        } else {
            // fall thru, i.e. branch
        }
        core.PC               = Aex;
        core.right_instr_flag = false;
        break;

    case 0300: // пб, uj
        Aex                   = ADDR(addr + core.M[reg]);
        core.PC               = Aex;
        core.right_instr_flag = false;
        if (reg != 0 && addr == 0) {
            machine.set_after_return();
        }
        break;

    case 0310: // пв, vjm
        Aex                   = addr;
        core.M[reg]           = nextpc;
        core.M[0]             = 0;
        core.PC               = addr;
        core.right_instr_flag = false;
        machine.set_after_call();
        break;

    case 0320: // выпр, iret
        throw Exception("Illegal instruction 32 выпр/iret");

    case 0330: // стоп, stop
        // We are done.
        return true;

    case 0340: // пио, vzm
    branch_zero:
        Aex = addr;
        if (!core.M[reg]) {
            core.PC               = addr;
            core.right_instr_flag = false;
        }
        break;

    case 0350: // пино, v1m
        Aex = addr;
        if (core.M[reg]) {
            core.PC               = addr;
            core.right_instr_flag = false;
        }
        break;

    case 0360: // э36, *36
        // Как ПИО, но с выталкиванием БРЗ.
        goto branch_zero;

    case 0370: // цикл, vlm
        Aex = addr;
        if (!core.M[reg])
            break;
        core.M[reg]           = ADDR(core.M[reg] + 1);
        core.PC               = addr;
        core.right_instr_flag = false;
        break;

    default:
        // Unknown instruction - cannot happen.
        throw Exception("Unknown instruction");
    }

    if (next_mod != 0) {
        // Модификация адреса следующей команды.
        core.MOD           = next_mod;
        core.apply_mod_reg = true;
    } else {
        core.apply_mod_reg = false;
    }

    return false;
}
