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

class Machine;
class Memory;

//
// Internal state of the processor.
//
struct CoreState {
    unsigned PC;           // program counter СчАС
    uint64_t ACC;          // accumulator
    uint64_t RMR;          // регистр младших разрядов
    unsigned M[16 + 1];    // registers modifiers
    unsigned RAU;          // ALU mode (режим АУ)
    bool right_instr_flag; // execute right half of the word (ПрК)
    bool apply_mod_reg;    // modify address by register M[16] (ПрИК)
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

    // Internal exceptions.
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
    void run();

    // Simulate one instruction.
    void step();

    // Execute extracode.
    void extracode(unsigned opcode);

    // Memory access.
    uint64_t mem_fetch(unsigned addr);
    uint64_t mem_load(unsigned addr);
    void mem_store(unsigned addr, uint64_t val);

    // Set register value.
    void set_pc(unsigned val) { core.PC = val; }
    void set_m(unsigned index, unsigned val) { core.M[index] = val; }
    void set_rau(unsigned val) { core.RAU = val; }
    void set_acc(uint64_t val) { core.ACC = val; }

    // Get register value.
    unsigned get_pc() { return core.PC; }
    unsigned get_m(unsigned index) { return core.M[index]; }
    unsigned get_rau() { return core.RAU; }
    uint64_t get_acc() { return core.ACC; }
    uint64_t get_rmr() { return core.RMR; }

    // ALU register type.
    struct AluReg {
        int64_t mantissa;  // Note: signed value
        unsigned exponent; // offset by 64
    };

    // Arithmetics.
    void arith_add(uint64_t val, int negate_acc, int negate_val);
    void arith_normalize_and_round(AluReg acc, uint64_t mr, int rnd_rq);
    void arith_add_exponent(int val);
    void arith_change_sign(int negate_acc);
    void arith_multiply(uint64_t val);
    void arith_divide(uint64_t val);
    void arith_shift(int i);
};

#endif // DUBNA_PROCESSOR_H
