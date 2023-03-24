/*
 * Public interface to the SVS processor
 *
 * Copyright (c) 2022 Leonid Broukhis, Serge Vakulenko
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#ifndef __EL_SVS_API_H
#define __EL_SVS_API_H
#include <stdint.h>

/*!
 *  Status codes
 */
typedef enum {
    ESS_OK = 0,
    ESS_HALT,                          // Останов
    ESS_IBKPT,                         // Точка останова
    ESS_RWATCH,                        // Точка останова по считыванию
    ESS_WWATCH,                        // Точка останова по записи
    ESS_RUNOUT,                        // Выход за пределы памяти
    ESS_BADCMD,                        // Запрещенная команда
    ESS_INSN_CHECK,                    // Контроль команды
    ESS_INSN_PROT,                     // Команда в чужом листе
    ESS_OPERAND_PROT,                  // Число в чужом листе
    ESS_RAM_CHECK,                     // Контроль числа МОЗУ
    ESS_CACHE_CHECK,                   // Контроль числа БРЗ
    ESS_OVFL,                          // Переполнение АУ
    ESS_DIVZERO,                       // Деление на нуль
    ESS_DOUBLE_INTR,                   // Двойное внутреннее прерывание
    ESS_DRUMINVDATA,                   // Чтение неформатированного барабана
    ESS_DISKINVDATA,                   // Чтение неформатированного диска
    ESS_INSN_ADDR_MATCH,               // Останов по КРА
    ESS_LOAD_ADDR_MATCH,               // Останов по считыванию
    ESS_STORE_ADDR_MATCH,              // Останов по записи
    ESS_UNIMPLEMENTED,                 // Не реализовано
} ElSvsStatus;

/*!
 *  Interface functions
 */
struct ElSvsProcessor;

/*
 * Instantiate a processor.
 */
struct ElSvsProcessor *ElSvsAllocate(int cpu_index);

/*
 * Run simulation.
 */
ElSvsStatus ElSvsSimulate(struct ElSvsProcessor *cpu);

/*
 * Enable/disable tracing.
 * The mode string can have the following options:
 * i - trace execution of cpu instructions
 * e - trace extracodes only (except э75)
 * f - trace instruction fetch from memory
 * m - trace data load and store from/to memory
 * x - trace exceptions
 * r - trace hw registers
 */
void ElSvsSetTrace(struct ElSvsProcessor *cpu, const char *trace_mode, const char *filename);

/*
 * Set register value.
 */
void ElSvsSetPC(struct ElSvsProcessor *cpu, unsigned val);
void ElSvsSetM(struct ElSvsProcessor *cpu, unsigned index, unsigned val);
void ElSvsSetPult(struct ElSvsProcessor *cpu, unsigned index, uint64_t val);
void ElSvsSetAcc(struct ElSvsProcessor *cpu, uint64_t val);
void ElSvsSetRAU(struct ElSvsProcessor *cpu, unsigned val);

/*
 * Get register value.
 */
unsigned ElSvsGetPC(struct ElSvsProcessor *cpu);
unsigned ElSvsGetM(struct ElSvsProcessor *cpu, unsigned index);
uint64_t ElSvsGetAcc(struct ElSvsProcessor *cpu);
uint64_t ElSvsGetRMR(struct ElSvsProcessor *cpu);
unsigned ElSvsGetRAU(struct ElSvsProcessor *cpu);

/*
 * Convert assembly source code into binary word.
 */
uint64_t ElSvsAsm(const char *source);

#endif // __EL_SVS_API_H
