//
// BESM-6 processor unit.
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
#include <string>

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

    void set_additive()
    {
        RAU &= ~RAU_MODE;
        RAU |= RAU_ADD;
    }

    void set_multiplicative()
    {
        RAU &= ~RAU_MODE;
        RAU |= RAU_MULT;
    }

    void set_logical()
    {
        RAU &= ~RAU_MODE;
        RAU |= RAU_LOG;
    }
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
    struct CoreState core {};

    // Previous state, for tracing.
    struct CoreState prev {};

    unsigned RK{};    // регистр команд
    unsigned Aex{};   // executive address
    int corr_stack{}; // stack correction on exception

    // Intercept divzero/overflow.
    unsigned intercept_count{};     // intercept this many times
    unsigned intercept_addr{ 020 }; // jump to this address
    const std::string MSG_ARITH_OVERFLOW = "Arithmetic overflow";
    const std::string MSG_ARITH_DIVZERO  = "Division by zero";

    // Extracodes.
    void extracode(unsigned opcode);
    void e50();
    void e57();
    void e61();
    void e63();
    void e64();
    void e65();
    void e67();
    void e70();
    void e71();
    void e72();
    void e75();
    void e76();

    // Internals of the print extracode.
    unsigned e64_print_gost(unsigned addr0, unsigned addr1);
    unsigned e64_print_octal(unsigned addr0, unsigned addr1, unsigned digits, unsigned width,
                             unsigned repeat);
    unsigned e64_print_instructions(unsigned addr0, unsigned addr1, unsigned width,
                                    unsigned repeat);
    unsigned e64_print_real(unsigned addr0, unsigned addr1, unsigned digits, unsigned width,
                            unsigned repeat);
    unsigned e64_print_itm(unsigned addr0, unsigned addr1);
    unsigned e64_print_hex(unsigned addr0, unsigned addr1, unsigned digits, unsigned width,
                           unsigned repeat);
    void e64_print_cmd(unsigned cmd);
    void e64_putchar(int ch);
    void e64_emit_line();
    void e64_flush_line();
    void e64_finish();
    std::string e64_line;
    int e64_skip_lines{0};
    unsigned e64_position{};
    unsigned e64_line_count{};
    bool e64_line_dirty{};
    bool e64_overprint{};

public:
    // Exception for unexpected situations.
    class Exception : public std::exception {
    private:
        std::string message;

    public:
        explicit Exception(const std::string &m) : message(m) {}
        const char *what() const noexcept override { return message.c_str(); }
    };

    // Constructor.
    Processor(Machine &machine, Memory &memory);

    // Reset to initial state.
    void reset();

    // Simulate one instruction.
    // Return true when the processor is stopped.
    bool step();

    // Stack correction in case of exception.
    void stack_correction();

    // Finalize the processor.
    void finish();

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
    bool on_right_instruction() const { return core.right_instr_flag; }

    // Arithmetics.
    void arith_add(Word val, bool negate_acc, bool negate_val);
    void arith_normalize_and_round(MantissaExponent acc, Word mr, bool round_flag);
    void arith_add_exponent(int val);
    void arith_change_sign(bool negate_acc);
    void arith_multiply(Word val);
    void arith_divide(Word val);
    void arith_shift(int nbits);

    // Intercept ofvl/divzero exception, when enabled.
    bool intercept(const std::string &message);

    // Print trace info.
    void print_instruction();
    void print_registers();
};

#endif // DUBNA_PROCESSOR_H
