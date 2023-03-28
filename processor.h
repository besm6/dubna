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
#ifndef DUBNA_PROCESSOR_H
#define DUBNA_PROCESSOR_H

#include <cstdint>
#include <setjmp.h>
#include "besm6_arch.h"
#include "extracode.h"

class Machine;
class Memory;
class MantissaExponent;

//
// Internal state of the processor.
//
struct CoreState {
    unsigned PC;           // program counter СчАС
    Word ACC;              // accumulator
    Word RMR;              // регистр младших разрядов
    unsigned M[16];        // registers modifiers
    unsigned MOD;          // MOD register
    unsigned RAU;          // ALU mode (режим АУ)
    bool right_instr_flag; // execute right half of the word (ПрК)
    bool apply_mod_reg;    // modify address by register M[16] (ПрИК)

    // Check and modify ALU mode.
    bool is_additive() const { return RAU & RAU_ADD; }
    bool is_multiplicative() const { return (RAU & (RAU_ADD | RAU_MULT)) == RAU_MULT; }
    bool is_logical() const { return (RAU & RAU_MODE) == RAU_LOG; }
    void set_additive() { RAU &= ~RAU_MODE; RAU |= RAU_ADD; }
    void set_multiplicative() { RAU &= ~RAU_MODE; RAU |= RAU_MULT; }
    void set_logical() { RAU &= ~RAU_MODE; RAU |= RAU_LOG; }
};

//
// BESM-6 processor.
//
class Processor {
private:
    // Reference to the machine.
    Machine &machine;

    // 32K words of virtual memory.
    Memory &memory;

    // Current state.
    struct CoreState core{};

    // Previous state, for tracing.
    struct CoreState prev{};

    unsigned RK{};       // регистр команд
    unsigned Aex{};      // executive address
    jmp_buf exception{}; // jump here on exception
    int corr_stack{};    // stack correction on exception

    // Run status codes.
    enum {
        ESS_OK = 0,
        ESS_HALT,          // Останов
        ESS_BADCMD,        // Запрещенная команда
        ESS_OVFL,          // Переполнение АУ
        ESS_DIVZERO,       // Деление на нуль
        ESS_JUMPZERO,      // Переход на 0
        ESS_UNIMPLEMENTED, // Не реализовано
    };

public:
    // Constructor.
    Processor(Machine &machine, Memory &memory);

    // Reset to initial state.
    void reset();

    // Main instruction fetch/decode loop.
    // Return status code.
    int run();

    // Simulate one instruction.
    void step();

    // Memory access.
    Word mem_fetch(unsigned addr);
    Word mem_load(unsigned addr);
    void mem_store(unsigned addr, Word val);

    // Set register value.
    void set_pc(unsigned val) { core.PC = val; }
    void set_m(unsigned index, unsigned val) { core.M[index] = val; }
    void set_rau(unsigned val) { core.RAU = val; }
    void set_acc(Word val) { core.ACC = val; }

    // Get register value.
    unsigned get_pc() const { return core.PC; }
    unsigned get_m(unsigned index) const { return core.M[index]; }
    unsigned get_rau() const { return core.RAU; }
    Word get_acc() const { return core.ACC; }
    Word get_rmr() const { return core.RMR; }

    // Arithmetics.
    void arith_add(Word val, bool negate_acc, bool negate_val);
    void arith_normalize_and_round(MantissaExponent acc, Word mr, bool round_flag);
    void arith_add_exponent(int val);
    void arith_change_sign(bool negate_acc);
    void arith_multiply(Word val);
    void arith_divide(Word val);
    void arith_shift(int nbits);

    // Print trace info.
    void print_instruction();
    void print_registers();

    // Extracodes.
    void extracode(unsigned opcode);
    void e70();
};

#endif // DUBNA_PROCESSOR_H
