/*
 * SVS CPU simulator.
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
#define _DEFAULT_SOURCE
#include "el_master_api.h"
#include "el_svs_api.h"
#include "el_svs_internal.h"
#include <math.h>
#include <float.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>

//
// Wired (non-registered) bits of interrupt registers (RPR and GRVP)
// cannot be cleared by writing to the RPR and must be cleared by clearing
// the registers generating the corresponding interrupts.
//
#define RPR_WIRED_BITS (0)

#define GRVP_WIRED_BITS (0)

static const char *sim_stop_messages[] = {
    "Неизвестная ошибка",                 // Unknown error
    "Останов",                            // STOP
    "Точка останова",                     // Emulator breakpoint
    "Точка останова по считыванию",       // Emulator read watchpoint
    "Точка останова по записи",           // Emulator write watchpoint
    "Выход за пределы памяти",            // Run out end of memory
    "Запрещенная команда",                // Invalid instruction
    "Контроль команды",                   // A data-tagged word fetched
    "Команда в чужом листе",              // Paging error during fetch
    "Число в чужом листе",                // Paging error during load/store
    "Контроль числа МОЗУ",                // RAM parity error
    "Контроль числа БРЗ",                 // Write cache parity error
    "Переполнение АУ",                    // Arith. overflow
    "Деление на нуль",                    // Division by zero or denorm
    "Двойное внутреннее прерывание",      // SIMH: Double internal interrupt
    "Чтение неформатированного барабана", // Reading unformatted drum
    "Чтение неформатированного диска",    // Reading unformatted disk
    "Останов по КРА",                     // Hardware breakpoint
    "Останов по считыванию",              // Load watchpoint
    "Останов по записи",                  // Store watchpoint
    "Не реализовано",                     // Unimplemented I/O or special reg. access
};

//
// Set value of the Pult register.
//
void ElSvsSetPult(struct ElSvsProcessor *cpu, unsigned index, uint64_t val)
{
    if (index > 0 && index < 010)
        cpu->pult[index] = val;
}

//
// Reset routine
//
void cpu_reset(struct ElSvsProcessor *cpu, unsigned cpu_index)
{
    cpu->index = cpu_index;
    cpu->core.ACC = 0;
    cpu->core.RMR = 0;
    cpu->core.RAU = 0;
    cpu->core.RUU = RUU_EXTRACODE | RUU_AVOST_DISABLE;

    memset(cpu->core.M, 0, sizeof(cpu->core.M));

    // Регистр 17: БлП, БлЗ, ПОП, ПОК, БлПр
    cpu->core.M[PSW] = PSW_MMAP_DISABLE | PSW_PROT_DISABLE | PSW_INTR_HALT |
        PSW_CHECK_HALT | PSW_INTR_DISABLE;

    // Регистр 23: БлП, БлЗ, РежЭ, БлПр
    cpu->core.M[SPSW] = SPSW_MMAP_DISABLE | SPSW_PROT_DISABLE | SPSW_EXTRACODE |
        SPSW_INTR_DISABLE;

    cpu->core.RZ = 0;
    memset(cpu->core.RP, 0, sizeof(cpu->core.RP));
    memset(cpu->core.RPS, 0, sizeof(cpu->core.RPS));

    cpu->core.RPR = 0;
    cpu->core.GRM = 0;
    cpu->core.PP = 0;
    cpu->core.OPP = 0;
    cpu->core.POP = 0;
    cpu->core.OPOP = 0;
    cpu->core.RKP = 0;

    cpu->core.PC = 1;

    if (cpu->trace_instructions | cpu->trace_extracodes | cpu->trace_fetch |
        cpu->trace_memory | cpu->trace_exceptions | cpu->trace_registers) {
        fprintf(cpu->log_output, "cpu%d --- Reset\n", cpu->index);
    }
    //TODO: mpd_reset(cpu);
}

//
// Set register value.
//
void ElSvsSetPC(struct ElSvsProcessor *cpu, unsigned val)
{
    cpu->core.PC = val;
}

void ElSvsSetM(struct ElSvsProcessor *cpu, unsigned index, unsigned val)
{
    cpu->core.M[index] = val;
}

void ElSvsSetRAU(struct ElSvsProcessor *cpu, unsigned val)
{
    cpu->core.RAU = val;
}

void ElSvsSetAcc(struct ElSvsProcessor *cpu, uint64_t val)
{
    cpu->core.ACC = val;
}

//
// Get register value.
//
unsigned ElSvsGetPC(struct ElSvsProcessor *cpu)
{
    return cpu->core.PC;
}

unsigned ElSvsGetM(struct ElSvsProcessor *cpu, unsigned index)
{
    return cpu->core.M[index];
}

uint64_t ElSvsGetAcc(struct ElSvsProcessor *cpu)
{
    return cpu->core.ACC;
}

uint64_t ElSvsGetRMR(struct ElSvsProcessor *cpu)
{
    return cpu->core.RMR;
}

unsigned ElSvsGetRAU(struct ElSvsProcessor *cpu)
{
    return cpu->core.RAU;
}

//
// Request routine
//
void cpu_req(struct ElSvsProcessor *cpu)
{
    if (cpu->trace_instructions | cpu->trace_extracodes | cpu->trace_fetch |
        cpu->trace_memory | cpu->trace_exceptions | cpu->trace_registers) {
        fprintf(cpu->log_output, "cpu%d --- Request from control panel\n", cpu->index);
    }
    cpu->core.GRVP |= GRVP_PANEL_REQ;
}

//
// Enable/disable tracing.
//
void ElSvsSetTrace(struct ElSvsProcessor *cpu, const char *trace_mode, const char *filename)
{
    if (cpu->log_output != stdout) {
        // Close previous log file.
        fclose(cpu->log_output);
        cpu->log_output = stdout;
    }

    // Disable all trace options.
    cpu->trace_instructions = false;
    cpu->trace_extracodes = false;
    cpu->trace_fetch = false;
    cpu->trace_memory = false;
    cpu->trace_exceptions = false;
    cpu->trace_registers = false;

    if (trace_mode && trace_mode[0]) {
        // Parse the mode string and enable all requested trace flags.
        int i;
        for (i = 0; trace_mode[i]; i++) {
            switch (trace_mode[i]) {
            case 'i': cpu->trace_instructions = true; break;
            case 'e': cpu->trace_extracodes = true; break;
            case 'f': cpu->trace_fetch = true; break;
            case 'm': cpu->trace_memory = true; break;
            case 'x': cpu->trace_exceptions = true; break;
            case 'r': cpu->trace_registers = true; break;
            default:
                fprintf(stderr, "Wrong trace option: %c\n", trace_mode[i]);
                exit(1);
            }
        }

        if (filename && filename[0]) {
            // Open new log file.
            cpu->log_output = fopen(filename, "a");
            if (!cpu->log_output) {
                perror(filename);
                exit(1);
            }
            setlinebuf(cpu->log_output);
        }
    }
}

//
// Instantiate a processor.
//
struct ElSvsProcessor *ElSvsAllocate(int cpu_index)
{
    struct ElSvsProcessor *cpu = calloc(1, sizeof(struct ElSvsProcessor));

    if (!cpu) {
        perror(__func__);
        abort();
    }
    cpu_reset(cpu, cpu_index);
    cpu->log_output = stdout;
    return cpu;
}

//
// Write Unicode symbol to file.
// Convert to UTF-8 encoding:
// 00000000.0xxxxxxx -> 0xxxxxxx
// 00000xxx.xxyyyyyy -> 110xxxxx, 10yyyyyy
// xxxxyyyy.yyzzzzzz -> 1110xxxx, 10yyyyyy, 10zzzzzz
//
void utf8_putc(unsigned ch, FILE *fout)
{
    if (ch < 0x80) {
        putc(ch, fout);
        return;
    }
    if (ch < 0x800) {
        putc(ch >> 6 | 0xc0, fout);
        putc((ch & 0x3f) | 0x80, fout);
        return;
    }
    putc(ch >> 12 | 0xe0, fout);
    putc(((ch >> 6) & 0x3f) | 0x80, fout);
    putc((ch & 0x3f) | 0x80, fout);
}

//
// Команда "рег"
//
static void cmd_002(struct ElSvsProcessor *cpu)
{
    //printf("--- рег %03o", cpu->Aex & 0377);

    switch (cpu->Aex & 0377) {

    case 020: case 021: case 022: case 023:
    case 024: case 025: case 026: case 027:
        // Запись в регистры приписки режима пользователя
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Установка приписки пользователя\n", cpu->index);
        mmu_set_rp(cpu, cpu->Aex & 7, cpu->core.ACC, 0);
        break;

    case 030: case 031: case 032: case 033:
        // Запись в регистры защиты
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Запись в регистр защиты\n", cpu->index);
        mmu_set_protection(cpu, cpu->Aex & 3, cpu->core.ACC);
        break;

    case 034:
        // Запись в регистр конфигурации оперативной памяти
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Запись конфигурации оперативной памяти\n", cpu->index);
        // игнорируем
        break;

    case 035:
        // Запись в сигнал контроля оперативной памяти
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Запись в сигнал контроля оперативной памяти\n", cpu->index);
        // игнорируем
        break;

    case 0235:
        // Чтение сигнала контроля от оперативной памяти
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Чтение сигнала контроля оперативной памяти\n", cpu->index);
        cpu->core.ACC = 0;
        break;

    case 0236:
        // Считывание сигналов запрета запроса в МОП от коммутаторов памяти
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Чтение ЗЗ\n", cpu->index);
        cpu->core.ACC = 0; // не используем
        break;

    case 037:
        // Гашение регистра внутренних прерываний
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Гашение РПР\n", cpu->index);
        cpu->core.RPR &= cpu->core.ACC | RPR_WIRED_BITS;
        break;

    case 0237:
        // Чтение главного регистра прерываний
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Чтение ГРП\n", cpu->index);
        cpu->core.ACC = cpu->core.RPR;
        break;

    case 044:
        // Запись в регистр тега
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Установка тега\n", cpu->index);
        cpu->core.TagR = cpu->core.ACC;
        break;

    case 0244:
        // Чтение регистра тега
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Чтение регистра тега\n", cpu->index);
        cpu->core.ACC = cpu->core.TagR;
        break;

    case 0245:
        // Чтение регистра ТЕГБРЧ
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Чтение ТЕГБРЧ\n", cpu->index);
        cpu->core.ACC = 0; //TODO
        break;

    case 046:
        // Запись маски внешних прерываний
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Установка ГРМ\n", cpu->index);
        cpu->core.GRM = cpu->core.ACC;
        break;

    case 0246:
        // Чтение маски внешних прерываний
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Чтение ГРМ\n", cpu->index);
        cpu->core.ACC = cpu->core.GRM;
        break;

    case 047:
        // Clearing the external interrupt register:
        // it is impossible to clear wired (stateless) bits this way
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Гашение РВП\n", cpu->index);
        cpu->core.GRVP &= cpu->core.ACC | GRVP_WIRED_BITS;
        break;

    case 0247:
        // Чтение регистра внешних прерываний
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Чтение РВП\n", cpu->index);
        cpu->core.ACC = cpu->core.GRVP;
        break;

    case 050:
        // Запись в регистр прерываний процессорам
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Запись в ПП\n", cpu->index);
        cpu->core.PP = cpu->core.ACC & (CONF_IOM_MASK | CONF_CPU_MASK | CONF_DATA_MASK);
        if (cpu->core.ACC & CONF_MT) {
            // Передача младшей половины байта.
            //TODO: mpd_send_nibble(cpu, CONF_GET_DATA(cpu->core.PP));
        }
        if (cpu->core.ACC & CONF_MR) {
            // Подтверждение считывания принятого байта.
            //TODO: mpd_receive_update(cpu);
        }
        if (cpu->core.ACC & CONF_IOM1) {
            // Запрос к ПВВ.
            //TODO: ПВВ 2...4
            //TODO: iom_request(cpu->index);
        }
        break;

    case 0250:
        // Чтение номера процессора
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Чтение номера процессора\n", cpu->index);
        cpu->core.ACC = cpu->index;
        break;

    case 051:
        // Запись в регистр ответов процессорам
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Запись в ОПП\n", cpu->index);
        cpu->core.OPP = cpu->core.ACC & (CONF_IOM_MASK | CONF_CPU_MASK | CONF_DATA_MASK);
        if (cpu->core.ACC & CONF_MT) {
            // Передача старшей половины байта.
            //TODO: mpd_send_nibble(cpu, CONF_GET_DATA(cpu->core.OPP));
        }
        if (cpu->core.ACC & CONF_IOM_RESET) {
            // Сброс ПВВ.
            //TODO: iom_reset(cpu->index);
        }
        break;

    case 052:
        // Гашение регистра прерываний от процессоров
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Гашение ПОП\n", cpu->index);
        // Оставляем бит передачи МПД.
        cpu->core.POP &= cpu->core.ACC | CONF_MT;
        break;

    case 0252:
        // Чтение регистра прерываний от процессоров
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Чтение ПОП\n", cpu->index);
        cpu->core.ACC = cpu->core.POP;
        break;

    case 053:
        // Гашение регистра ответов от процессоров
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Гашение ОПОП\n", cpu->index);
        cpu->core.OPOP &= cpu->core.ACC;
        break;

    case 0253:
        // Чтение регистра ответов от процессоров
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Чтение ОПОП\n", cpu->index);
        cpu->core.ACC = cpu->core.OPOP;
        break;

    case 054:
        // Запись в регистр конфигурации процессора
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Установка конфигурации процессора\n", cpu->index);
        cpu->core.RKP = cpu->core.ACC & (CONF_IOM_MASK | CONF_CPU_MASK | CONF_MR | CONF_MT);
        break;

    case 0254:
        // Чтение регистра конфигурации процессора
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Чтение регистра конфигурации процессора\n", cpu->index);
        cpu->core.ACC = cpu->core.RKP;
        break;

    case 055:
        // Запись в регистр аварии процессоров
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Запись в регистр аварии процессоров\n", cpu->index);
        // игнорируем
        break;

    case 0255:
        // Чтение регистра аварии процессоров
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Чтение регистра аварии процессоров\n", cpu->index);
        cpu->core.ACC = 0;
        break;

    case 056:
        // Запись в регистр часов
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Установка часов\n", cpu->index);
        //TODO
        break;

    case 0256:
        // Чтение регистра часов
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Чтение регистра часов\n", cpu->index);
        cpu->core.ACC = 0; //TODO
        break;

    case 057:
        // Запись в регистр таймера
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Установка таймера\n", cpu->index);
        //TODO
        break;

    case 0257:
        // Чтение регистра таймера
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Чтение регистра таймера\n", cpu->index);
        cpu->core.ACC = 0; //TODO
        break;

    case 060: case 061: case 062: case 063:
    case 064: case 065: case 066: case 067:
        // Запись в регистры приписки супервизора
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Установка приписки супервизора\n", cpu->index);
        mmu_set_rp(cpu, cpu->Aex & 7, cpu->core.ACC, 1);
        break;

    case 0100: case 0101: case 0102: case 0103:
    case 0104: case 0105: case 0106: case 0107:
        //
        // Бит 1: управление блокировкой режима останова БРО.
        // Биты 2 и 3 - признаки формирования контрольных
        // разрядов (ПКП и ПКЛ).
        //
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Установка режимов УУ\n", cpu->index);

        if (cpu->Aex & 1) cpu->core.RUU |= RUU_AVOST_DISABLE;
        else              cpu->core.RUU &= ~RUU_AVOST_DISABLE;

        if (cpu->Aex & 2) cpu->core.RUU |= RUU_CHECK_RIGHT;
        else              cpu->core.RUU &= ~RUU_CHECK_RIGHT;

        if (cpu->Aex & 4) cpu->core.RUU |= RUU_CHECK_LEFT;
        else              cpu->core.RUU &= ~RUU_CHECK_LEFT;
        break;

    case 0140:
        // Сброс контрольных признаков (СКП).
        if (cpu->trace_instructions | cpu->trace_registers)
            fprintf(cpu->log_output, "cpu%d --- Сброс контрольных признаков\n",
                cpu->index);
        //TODO
        break;

    default:
#if 0
        if ((cpu->Aex & 0340) == 0140) {
            // TODO: watchdog reset mechanism
            longjmp(cpu->exception, ESS_UNIMPLEMENTED);
        }
#endif
        // Неиспользуемые адреса
        printf("--- %05o%s: РЕГ %o - неизвестный спец.регистр",
            cpu->core.PC, (cpu->core.RUU & RUU_RIGHT_INSTR) ? "п" : "л", cpu->Aex);
        break;
    }
}

static int is_extracode(int opcode)
{
    switch (opcode) {
    case 050: case 051: case 052: case 053: // э50...э77 кроме э75
    case 054: case 055: case 056: case 057:
    case 060: case 061: case 062: case 063:
    case 064: case 065: case 066: case 067:
    case 070: case 071: case 072: case 073:
    case 074: case 076: case 077:
    case 0200:                              // э20
    case 0210:                              // э21
        return 1;
    }
    return 0;
}

//
// Execute one instruction, placed on address PC:RUU_RIGHT_INSTR.
// When stopped, perform a longjmp to cpu->exception,
// sending a stop code.
//
void cpu_one_instr(struct ElSvsProcessor *cpu)
{
    int reg, opcode, addr, paddr, nextpc, next_mod;
    uint64_t word;

    cpu->corr_stack = 0;
    word = mmu_fetch(cpu, cpu->core.PC, &paddr);
    if (cpu->core.RUU & RUU_RIGHT_INSTR)
        cpu->RK = (uint32_t)word;         // get right instruction
    else
        cpu->RK = (uint32_t)(word >> 24); // get left instruction

    cpu->RK &= BITS(24);

    reg = cpu->RK >> 20;
    if (cpu->RK & BBIT(20)) {
        addr = cpu->RK & BITS(15);
        opcode = (cpu->RK >> 12) & 0370;
    } else {
        addr = cpu->RK & BITS(12);
        if (cpu->RK & BBIT(19))
            addr |= 070000;
        opcode = (cpu->RK >> 12) & 077;
    }

    // Трассировка команды: адрес, код и мнемоника.
    if (cpu->trace_instructions ||
        (cpu->trace_extracodes && is_extracode(opcode))) {
        svs_trace_opcode(cpu, paddr);
    }

    nextpc = ADDR(cpu->core.PC + 1);
    if (cpu->core.RUU & RUU_RIGHT_INSTR) {
        cpu->core.PC += 1;                               // increment PC
        cpu->core.RUU &= ~RUU_RIGHT_INSTR;
    } else {
        cpu->core.RUU |= RUU_RIGHT_INSTR;
    }

    if (cpu->core.RUU & RUU_MOD_RK) {
        addr = ADDR(addr + cpu->core.M[MOD]);
    }
    next_mod = 0;

    switch (opcode) {
    case 000:                                       // зп, atx
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        mmu_store(cpu, cpu->Aex, cpu->core.ACC);
        if (! addr && reg == 017)
            cpu->core.M[017] = ADDR(cpu->core.M[017] + 1);
        break;
    case 001:                                       // зпм, stx
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        mmu_store(cpu, cpu->Aex, cpu->core.ACC);
        cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
        cpu->corr_stack = 1;
        cpu->core.ACC = mmu_load(cpu, cpu->core.M[017]);
        cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    case 002:                                       // рег, mod
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        if (! IS_SUPERVISOR(cpu->core.RUU))
            longjmp(cpu->exception, ESS_BADCMD);
        cmd_002(cpu);
        // Режим АУ - логический, если операция была "чтение"
        if (cpu->Aex & 0200)
            cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    case 003:                                       // счм, xts
        mmu_store(cpu, cpu->core.M[017], cpu->core.ACC);
        cpu->core.M[017] = ADDR(cpu->core.M[017] + 1);
        cpu->corr_stack = -1;
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.ACC = mmu_load(cpu, cpu->Aex);
        cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    case 004:                                       // сл, a+x
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        svs_add(cpu, mmu_load(cpu, cpu->Aex), 0, 0);
        cpu->core.RAU = SET_ADDITIVE(cpu->core.RAU);
        break;
    case 005:                                       // вч, a-x
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        svs_add(cpu, mmu_load(cpu, cpu->Aex), 0, 1);
        cpu->core.RAU = SET_ADDITIVE(cpu->core.RAU);
        break;
    case 006:                                       // вчоб, x-a
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        svs_add(cpu, mmu_load(cpu, cpu->Aex), 1, 0);
        cpu->core.RAU = SET_ADDITIVE(cpu->core.RAU);
        break;
    case 007:                                       // вчаб, amx
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        svs_add(cpu, mmu_load(cpu, cpu->Aex), 1, 1);
        cpu->core.RAU = SET_ADDITIVE(cpu->core.RAU);
        break;
    case 010:                                       // сч, xta
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.ACC = mmu_load(cpu, cpu->Aex);
        cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    case 011:                                       // и, aax
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.ACC &= mmu_load(cpu, cpu->Aex);
        cpu->core.RMR = 0;
        cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    case 012:                                       // нтж, aex
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.RMR = cpu->core.ACC;
        cpu->core.ACC ^= mmu_load(cpu, cpu->Aex);
        cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    case 013:                                       // слц, arx
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.ACC += mmu_load(cpu, cpu->Aex);
        if (cpu->core.ACC & BIT49)
            cpu->core.ACC = (cpu->core.ACC + 1) & BITS48;
        cpu->core.RMR = 0;
        cpu->core.RAU = SET_MULTIPLICATIVE(cpu->core.RAU);
        break;
    case 014:                                       // знак, avx
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        svs_change_sign(cpu, mmu_load(cpu, cpu->Aex) >> 40 & 1);
        cpu->core.RAU = SET_ADDITIVE(cpu->core.RAU);
        break;
    case 015:                                       // или, aox
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.ACC |= mmu_load(cpu, cpu->Aex);
        cpu->core.RMR = 0;
        cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    case 016:                                       // дел, a/x
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        svs_divide(cpu, mmu_load(cpu, cpu->Aex));
        cpu->core.RAU = SET_MULTIPLICATIVE(cpu->core.RAU);
        break;
    case 017:                                       // умн, a*x
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        svs_multiply(cpu, mmu_load(cpu, cpu->Aex));
        cpu->core.RAU = SET_MULTIPLICATIVE(cpu->core.RAU);
        break;
    case 020:                                       // сбр, apx
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.ACC = svs_pack(cpu->core.ACC, mmu_load(cpu, cpu->Aex));
        cpu->core.RMR = 0;
        cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    case 021:                                       // рзб, aux
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.ACC = svs_unpack(cpu->core.ACC, mmu_load(cpu, cpu->Aex));
        cpu->core.RMR = 0;
        cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    case 022:                                       // чед, acx
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.ACC = svs_count_ones(cpu->core.ACC) + mmu_load(cpu, cpu->Aex);
        if (cpu->core.ACC & BIT49)
            cpu->core.ACC = (cpu->core.ACC + 1) & BITS48;
        cpu->core.RMR = 0;
        cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    case 023:                                       // нед, anx
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        if (cpu->core.ACC) {
            int n = svs_highest_bit(cpu->core.ACC);

            // "Остаток" сумматора, исключая бит,
            // номер которого определен, помещается в РМР,
            // начиная со старшего бита РМР.
            svs_shift(cpu, 48 - n);

            // Циклическое сложение номера со словом по Аисп.
            cpu->core.ACC = n + mmu_load(cpu, cpu->Aex);
            if (cpu->core.ACC & BIT49)
                cpu->core.ACC = (cpu->core.ACC + 1) & BITS48;
        } else {
            cpu->core.RMR = 0;
            cpu->core.ACC = mmu_load(cpu, cpu->Aex);
        }
        cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    case 024:                                       // слп, e+x
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        svs_add_exponent(cpu, (mmu_load(cpu, cpu->Aex) >> 41) - 64);
        cpu->core.RAU = SET_MULTIPLICATIVE(cpu->core.RAU);
        break;
    case 025:                                       // вчп, e-x
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        svs_add_exponent(cpu, 64 - (mmu_load(cpu, cpu->Aex) >> 41));
        cpu->core.RAU = SET_MULTIPLICATIVE(cpu->core.RAU);
        break;
    case 026: {                                     // сд, asx
        int n;
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        n = (mmu_load(cpu, cpu->Aex) >> 41) - 64;
        svs_shift(cpu, n);
        cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    }
    case 027:                                       // рж, xtr
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.RAU = (mmu_load(cpu, cpu->Aex) >> 41) & 077;
        break;
    case 030:                                       // счрж, rte
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.ACC = (uint64_t) (cpu->core.RAU & cpu->Aex & 0177) << 41;
        cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    case 031:                                       // счмр, yta
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        if (IS_LOGICAL(cpu->core.RAU)) {
            cpu->core.ACC = cpu->core.RMR;
        } else {
            uint64_t x = cpu->core.RMR;
            cpu->core.ACC = (cpu->core.ACC & ~BITS41) | (cpu->core.RMR & BITS40);
            svs_add_exponent(cpu, (cpu->Aex & 0177) - 64);
            cpu->core.RMR = x;
        }
        break;
    case 032:                                       // зпп, запись полноразрядная
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        if (! IS_SUPERVISOR(cpu->core.RUU))
            longjmp(cpu->exception, ESS_BADCMD);
        mmu_store64(cpu, cpu->Aex, (cpu->core.ACC << 16) |
            ((cpu->core.RMR >> 32) & BITS(16)));
        break;
    case 033:                                       // счп, считывание полноразрядное
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        if (! IS_SUPERVISOR(cpu->core.RUU))
            longjmp(cpu->exception, ESS_BADCMD);
//printf("--- счп %05o", cpu->Aex);
        cpu->core.ACC = mmu_load64(cpu, cpu->Aex, 1);
        cpu->core.RMR = (cpu->core.ACC & BITS(16)) << 32;
        cpu->core.ACC >>= 16;
        break;
    case 034:                                       // слпа, e+n
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        svs_add_exponent(cpu, (cpu->Aex & 0177) - 64);
        cpu->core.RAU = SET_MULTIPLICATIVE(cpu->core.RAU);
        break;
    case 035:                                       // вчпа, e-n
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        svs_add_exponent(cpu, 64 - (cpu->Aex & 0177));
        cpu->core.RAU = SET_MULTIPLICATIVE(cpu->core.RAU);
        break;
    case 036: {                                     // сда, asn
        int n;
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        n = (cpu->Aex & 0177) - 64;
        svs_shift(cpu, n);
        cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    }
    case 037:                                       // ржа, ntr
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.RAU = cpu->Aex & 077;
        break;
    case 040:                                       // уи, ati
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        if (IS_SUPERVISOR(cpu->core.RUU)) {
            int reg = cpu->Aex & 037;
            cpu->core.M[reg] = ADDR(cpu->core.ACC);
            //
            // breakpoint/watchpoint regs will match physical
            // or virtual addresses depending on the current
            // mapping mode.
            //
            if ((cpu->core.M[PSW] & PSW_MMAP_DISABLE) &&
                (reg == IBP || reg == DWP))
                cpu->core.M[reg] |= BBIT(16);

        } else
            cpu->core.M[cpu->Aex & 017] = ADDR(cpu->core.ACC);
        cpu->core.M[0] = 0;
        break;
    case 041: {                                     // уим, sti
        unsigned rg, ad;

        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        rg = cpu->Aex & (IS_SUPERVISOR(cpu->core.RUU) ? 037 : 017);
        ad = ADDR(cpu->core.ACC);
        if (rg != 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->core.ACC = mmu_load(cpu, rg != 017 ? cpu->core.M[017] : ad);
        cpu->core.M[rg] = ad;
        if ((cpu->core.M[PSW] & PSW_MMAP_DISABLE) && (rg == IBP || rg == DWP))
            cpu->core.M[rg] |= BBIT(16);
        cpu->core.M[0] = 0;
        cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    }
    case 042:                                       // счи, ita
load_modifier:
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.ACC = ADDR(cpu->core.M[cpu->Aex & (IS_SUPERVISOR(cpu->core.RUU) ? 037 : 017)]);
        cpu->core.RAU = SET_LOGICAL(cpu->core.RAU);
        break;
    case 043:                                       // счим, its
        mmu_store(cpu, cpu->core.M[017], cpu->core.ACC);
        cpu->core.M[017] = ADDR(cpu->core.M[017] + 1);
        goto load_modifier;
    case 044:                                       // уии, mtj
        cpu->Aex = addr;
        if (IS_SUPERVISOR(cpu->core.RUU)) {
transfer_modifier:
            cpu->core.M[cpu->Aex & 037] = cpu->core.M[reg];
            if ((cpu->core.M[PSW] & PSW_MMAP_DISABLE) &&
                ((cpu->Aex & 037) == IBP || (cpu->Aex & 037) == DWP))
                cpu->core.M[cpu->Aex & 037] |= BBIT(16);

        } else
            cpu->core.M[cpu->Aex & 017] = cpu->core.M[reg];
        cpu->core.M[0] = 0;
        break;
    case 045:                                       // сли, j+m
        cpu->Aex = addr;
        if ((cpu->Aex & 020) && IS_SUPERVISOR(cpu->core.RUU))
            goto transfer_modifier;
        cpu->core.M[cpu->Aex & 017] = ADDR(cpu->core.M[cpu->Aex & 017] + cpu->core.M[reg]);
        cpu->core.M[0] = 0;
        break;
    case 046:                                       // cоп, специальное обращение к памяти
        cpu->Aex = addr;
        if (! IS_SUPERVISOR(cpu->core.RUU))
            longjmp(cpu->exception, ESS_BADCMD);
//printf("--- соп %05o", cpu->Aex);
        cpu->core.ACC = mmu_load64(cpu, cpu->Aex, 0);
        cpu->core.RMR = (cpu->core.ACC & BITS(16)) << 32;
        cpu->core.ACC >>= 16;
        break;
    case 047:                                       // э47, x47
        cpu->Aex = addr;
        if (! IS_SUPERVISOR(cpu->core.RUU))
            longjmp(cpu->exception, ESS_BADCMD);
        cpu->core.M[cpu->Aex & 017] = ADDR(cpu->core.M[cpu->Aex & 017] + cpu->Aex);
        cpu->core.M[0] = 0;
        break;
    case 050: case 051: case 052: case 053:
    case 054: case 055: case 056: case 057:
    case 060: case 061: case 062: case 063:
    case 064: case 065: case 066: case 067:
    case 070: case 071: case 072: case 073:
    case 074: case 075: case 076: case 077:         // э50...э77
    case 0200:                                      // э20
    case 0210:                                      // э21
stop_as_extracode:
            cpu->Aex = ADDR(addr + cpu->core.M[reg]);
            // Адрес возврата из экстракода.
            cpu->core.M[ERET] = nextpc;
            // Сохранённые режимы УУ.
            cpu->core.M[SPSW] = (cpu->core.M[PSW] & (PSW_INTR_DISABLE | PSW_MMAP_DISABLE |
                                           PSW_PROT_DISABLE)) | IS_SUPERVISOR(cpu->core.RUU);
            // Текущие режимы УУ.
            cpu->core.M[PSW] = PSW_INTR_DISABLE | PSW_MMAP_DISABLE |
                          PSW_PROT_DISABLE | /*?*/ PSW_INTR_HALT;
            cpu->core.M[14] = cpu->Aex;
            cpu->core.RUU = SET_SUPERVISOR(cpu->core.RUU, SPSW_EXTRACODE);

            if (opcode <= 077)
                cpu->core.PC = 0500 + opcode;            // э50-э77
            else
                cpu->core.PC = 0540 + (opcode >> 3);     // э20, э21
            cpu->core.RUU &= ~RUU_RIGHT_INSTR;
            break;
    case 0220:                                      // мода, utc
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        next_mod = cpu->Aex;
        break;
    case 0230:                                      // мод, wtc
        if (! addr && reg == 017) {
            cpu->core.M[017] = ADDR(cpu->core.M[017] - 1);
            cpu->corr_stack = 1;
        }
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        next_mod = ADDR(mmu_load(cpu, cpu->Aex));
        break;
    case 0240:                                      // уиа, vtm
        cpu->Aex = addr;
        cpu->core.M[reg] = addr;
        cpu->core.M[0] = 0;
        if (IS_SUPERVISOR(cpu->core.RUU) && reg == 0) {
            cpu->core.M[PSW] &= ~(PSW_INTR_DISABLE |
                             PSW_MMAP_DISABLE | PSW_PROT_DISABLE);
            cpu->core.M[PSW] |= addr & (PSW_INTR_DISABLE |
                                   PSW_MMAP_DISABLE | PSW_PROT_DISABLE);
        }
        break;
    case 0250:                                      // слиа, utm
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.M[reg] = cpu->Aex;
        cpu->core.M[0] = 0;
        if (IS_SUPERVISOR(cpu->core.RUU) && reg == 0) {
            cpu->core.M[PSW] &= ~(PSW_INTR_DISABLE |
                             PSW_MMAP_DISABLE | PSW_PROT_DISABLE);
            cpu->core.M[PSW] |= addr & (PSW_INTR_DISABLE |
                                   PSW_MMAP_DISABLE | PSW_PROT_DISABLE);
        }
        break;
    case 0260:                                      // по, uza
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.RMR = cpu->core.ACC;
        if (IS_ADDITIVE(cpu->core.RAU)) {
            if (cpu->core.ACC & BIT41)
                break;
        } else if (IS_MULTIPLICATIVE(cpu->core.RAU)) {
            if (! (cpu->core.ACC & BIT48))
                break;
        } else if (IS_LOGICAL(cpu->core.RAU)) {
            if (cpu->core.ACC)
                break;
        } else
            break;
        cpu->core.PC = cpu->Aex;
        cpu->core.RUU &= ~RUU_RIGHT_INSTR;
        break;
    case 0270:                                      // пе, u1a
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.RMR = cpu->core.ACC;
        if (IS_ADDITIVE(cpu->core.RAU)) {
            if (! (cpu->core.ACC & BIT41))
                break;
        } else if (IS_MULTIPLICATIVE(cpu->core.RAU)) {
            if (cpu->core.ACC & BIT48)
                break;
        } else if (IS_LOGICAL(cpu->core.RAU)) {
            if (! cpu->core.ACC)
                break;
        } else {
            // fall thru, i.e. branch
        }
        cpu->core.PC = cpu->Aex;
        cpu->core.RUU &= ~RUU_RIGHT_INSTR;
        break;
    case 0300:                                      // пб, uj
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        cpu->core.PC = cpu->Aex;
        cpu->core.RUU &= ~RUU_RIGHT_INSTR;
        break;
    case 0310:                                      // пв, vjm
        cpu->Aex = addr;
        cpu->core.M[reg] = nextpc;
        cpu->core.M[0] = 0;
        cpu->core.PC = addr;
        cpu->core.RUU &= ~RUU_RIGHT_INSTR;
        break;
    case 0320:                                      // выпр, iret
        cpu->Aex = addr;
        if (! IS_SUPERVISOR(cpu->core.RUU)) {
            longjmp(cpu->exception, ESS_BADCMD);
        }
        cpu->core.M[PSW] = (cpu->core.M[PSW] & PSW_WRITE_WATCH) |
                      (cpu->core.M[SPSW] & (SPSW_INTR_DISABLE |
                                       SPSW_MMAP_DISABLE | SPSW_PROT_DISABLE));
        cpu->core.PC = cpu->core.M[(reg & 3) | 030];
        cpu->core.RUU &= ~RUU_RIGHT_INSTR;
        if (cpu->core.M[SPSW] & SPSW_RIGHT_INSTR)
            cpu->core.RUU |= RUU_RIGHT_INSTR;
        else
            cpu->core.RUU &= ~RUU_RIGHT_INSTR;
        cpu->core.RUU = SET_SUPERVISOR(cpu->core.RUU,
                                  cpu->core.M[SPSW] & (SPSW_EXTRACODE | SPSW_INTERRUPT));
        if (cpu->core.M[SPSW] & SPSW_MOD_RK)
            next_mod = cpu->core.M[MOD];
        break;
    case 0330:                                      // стоп, stop
        cpu->Aex = ADDR(addr + cpu->core.M[reg]);
        if (! IS_SUPERVISOR(cpu->core.RUU)) {
            if (cpu->core.M[PSW] & PSW_CHECK_HALT)
                break;
            else {
                opcode = 063;
                goto stop_as_extracode;
            }
        }
        longjmp(cpu->exception, ESS_HALT);
        break;
    case 0340:                                      // пио, vzm
branch_zero:
        cpu->Aex = addr;
        if (! cpu->core.M[reg]) {
            cpu->core.PC = addr;
            cpu->core.RUU &= ~RUU_RIGHT_INSTR;
        }
        break;
    case 0350:                                      // пино, v1m
        cpu->Aex = addr;
        if (cpu->core.M[reg]) {
            cpu->core.PC = addr;
            cpu->core.RUU &= ~RUU_RIGHT_INSTR;
        }
        break;
    case 0360:                                      // э36, *36
        // Как ПИО, но с выталкиванием БРЗ.
        goto branch_zero;
    case 0370:                                      // цикл, vlm
        cpu->Aex = addr;
        if (! cpu->core.M[reg])
            break;
        cpu->core.M[reg] = ADDR(cpu->core.M[reg] + 1);
        cpu->core.PC = addr;
        cpu->core.RUU &= ~RUU_RIGHT_INSTR;
        break;
    default:
        // Unknown instruction - cannot happen.
        longjmp(cpu->exception, ESS_HALT);
        break;
    }

    if (next_mod) {
        // Модификация адреса следующей команды.
        cpu->core.M[MOD] = next_mod;
        cpu->core.RUU |= RUU_MOD_RK;
    } else {
        cpu->core.RUU &= ~RUU_MOD_RK;
    }

    // Обновляем регистр внешних прерываний РВП.
    if (cpu->core.POP & cpu->core.RKP) {
        // Есть внешние прерывания.
        cpu->core.GRVP |= GRVP_REQUEST;
    } else {
        // Внешние прерывания отсутствуют.
        cpu->core.GRVP &= ~GRVP_REQUEST;
    }

    // Трассировка изменённых регистров.
    if (cpu->trace_registers) {
        svs_trace_registers(cpu);
    }
#if 0
    //TODO: обнаружение цикла "ЖДУ" диспака
    // Не находимся ли мы в цикле "ЖДУ" диспака?
    if (cpu->core.RUU == 047 && cpu->core.PC == 04440 && cpu->RK == 067704440) {
        //check_initial_setup();
        sim_idle(0, TRUE);
    }
#endif
}

//
// Операция прерывания 1: внутреннее прерывание.
// Описана в 9-м томе технического описания БЭСМ-6, страница 119.
//
void op_int_1(struct ElSvsProcessor *cpu, const char *msg)
{
    cpu->core.M[SPSW] = (cpu->core.M[PSW] & (PSW_INTR_DISABLE | PSW_MMAP_DISABLE |
                                   PSW_PROT_DISABLE)) | IS_SUPERVISOR(cpu->core.RUU);
    if (cpu->core.RUU & RUU_RIGHT_INSTR)
        cpu->core.M[SPSW] |= SPSW_RIGHT_INSTR;
    cpu->core.M[IRET] = cpu->core.PC;
    cpu->core.M[PSW] |= PSW_INTR_DISABLE | PSW_MMAP_DISABLE | PSW_PROT_DISABLE;
    if (cpu->core.RUU & RUU_MOD_RK) {
        cpu->core.M[SPSW] |= SPSW_MOD_RK;
        cpu->core.RUU &= ~RUU_MOD_RK;
    }
    cpu->core.PC = 0500;
    cpu->core.RUU &= ~RUU_RIGHT_INSTR;
    cpu->core.RUU = SET_SUPERVISOR(cpu->core.RUU, SPSW_INTERRUPT);
}

//
// Операция прерывания 2: внешнее прерывание.
// Описана в 9-м томе технического описания БЭСМ-6, страница 129.
//
void op_int_2(struct ElSvsProcessor *cpu)
{
    cpu->core.M[SPSW] = (cpu->core.M[PSW] & (PSW_INTR_DISABLE | PSW_MMAP_DISABLE |
                                   PSW_PROT_DISABLE)) | IS_SUPERVISOR(cpu->core.RUU);
    cpu->core.M[IRET] = cpu->core.PC;
    cpu->core.M[PSW] |= PSW_INTR_DISABLE | PSW_MMAP_DISABLE | PSW_PROT_DISABLE;
    if (cpu->core.RUU & RUU_MOD_RK) {
        cpu->core.M[SPSW] |= SPSW_MOD_RK;
        cpu->core.RUU &= ~RUU_MOD_RK;
    }
    cpu->core.PC = 0501;
    cpu->core.RUU &= ~RUU_RIGHT_INSTR;
    cpu->core.RUU = SET_SUPERVISOR(cpu->core.RUU, SPSW_INTERRUPT);
}

//
// Main instruction fetch/decode loop
//
ElSvsStatus ElSvsSimulate(struct ElSvsProcessor *cpu)
{
    int iintr = 0;

    // Трассировка начального состояния.
    if (cpu->trace_registers) {
        svs_trace_registers(cpu);
    }

    // Restore register state
    cpu->core.PC &= BITS(15);                            // mask PC
    mmu_setup(cpu);                                 // copy RP to TLB

    // An internal interrupt or user intervention
    ElSvsStatus r = setjmp(cpu->exception);
    if (r) {
        const char *message = sim_stop_messages[r];

        if (cpu->trace_instructions | cpu->trace_memory |
            cpu->trace_registers | cpu->trace_fetch) {
            fprintf(cpu->log_output, "cpu%d --- %s\n",
                cpu->index, message);
        }
        cpu->core.M[017] += cpu->corr_stack;

        //
        // ПоП и ПоК вызывают останов при любом внутреннем прерывании
        // или прерывании по контролю, соответственно.
        // Если произошёл останов по ПоП или ПоК,
        // то продолжение выполнения начнётся с команды, следующей
        // за вызвавшей прерывание. Как если бы кнопка "ТП" (тип
        // перехода) была включена. Подробнее на странице 119 ТО9.
        //
        switch (r) {
        default:
ret:        return r;
        case ESS_RWATCH:
        case ESS_WWATCH:
            // Step back one insn to reexecute it
            if (! (cpu->core.RUU & RUU_RIGHT_INSTR)) {
                --cpu->core.PC;
            }
            cpu->core.RUU ^= RUU_RIGHT_INSTR;
            goto ret;
        case ESS_BADCMD:
            if (cpu->core.M[PSW] & PSW_INTR_HALT)        // ПоП
                goto ret;
            op_int_1(cpu, sim_stop_messages[r]);
            // SPSW_NEXT_RK is not important for this interrupt
            cpu->core.RPR |= RPR_ILL_INSN;
            break;
        case ESS_INSN_CHECK:
            if (cpu->core.M[PSW] & PSW_CHECK_HALT)       // ПоК
                goto ret;
            op_int_1(cpu, sim_stop_messages[r]);
            // SPSW_NEXT_RK must be 0 for this interrupt; it is already
            cpu->core.RPR |= RPR_INSN_CHECK;
            break;
        case ESS_INSN_PROT:
            if (cpu->core.M[PSW] & PSW_INTR_HALT)        // ПоП
                goto ret;
            if (cpu->core.RUU & RUU_RIGHT_INSTR) {
                ++cpu->core.PC;
            }
            cpu->core.RUU ^= RUU_RIGHT_INSTR;
            op_int_1(cpu, sim_stop_messages[r]);
            // SPSW_NEXT_RK must be 1 for this interrupt
            cpu->core.M[SPSW] |= SPSW_NEXT_RK;
            cpu->core.RPR |= RPR_INSN_PROT;
            break;
        case ESS_OPERAND_PROT:
#if 0
// ДИСПАК держит признак ПоП установленным.
// При запуске СЕРП возникает обращение к чужому листу.
            if (cpu->core.M[PSW] & PSW_INTR_HALT)        // ПоП
                goto ret;
#endif
            if (cpu->core.RUU & RUU_RIGHT_INSTR) {
                ++cpu->core.PC;
            }
            cpu->core.RUU ^= RUU_RIGHT_INSTR;
            op_int_1(cpu, sim_stop_messages[r]);
            cpu->core.M[SPSW] |= SPSW_NEXT_RK;
            // The offending virtual page is in bits 5-9
            cpu->core.RPR |= RPR_OPRND_PROT;
            cpu->core.RPR = RPR_SET_PAGE(cpu->core.RPR, cpu->core.bad_addr);
            break;
        case ESS_RAM_CHECK:
            if (cpu->core.M[PSW] & PSW_CHECK_HALT)       // ПоК
                goto ret;
            op_int_1(cpu, sim_stop_messages[r]);
            // The offending interleaved block # is in bits 1-3.
            cpu->core.RPR |= RPR_CHECK | RPR_RAM_CHECK;
            cpu->core.RPR = RPR_SET_BLOCK(cpu->core.RPR, cpu->core.bad_addr);
            break;
        case ESS_CACHE_CHECK:
            if (cpu->core.M[PSW] & PSW_CHECK_HALT)       // ПоК
                goto ret;
            op_int_1(cpu, sim_stop_messages[r]);
            // The offending BRZ # is in bits 1-3.
            cpu->core.RPR |= RPR_CHECK;
            cpu->core.RPR &= ~RPR_RAM_CHECK;
            cpu->core.RPR = RPR_SET_BLOCK(cpu->core.RPR, cpu->core.bad_addr);
            break;
        case ESS_INSN_ADDR_MATCH:
            if (cpu->core.M[PSW] & PSW_INTR_HALT)        // ПоП
                goto ret;
            if (cpu->core.RUU & RUU_RIGHT_INSTR) {
                ++cpu->core.PC;
            }
            cpu->core.RUU ^= RUU_RIGHT_INSTR;
            op_int_1(cpu, sim_stop_messages[r]);
            cpu->core.M[SPSW] |= SPSW_NEXT_RK;
            cpu->core.RPR |= RPR_BREAKPOINT;
            break;
        case ESS_LOAD_ADDR_MATCH:
            if (cpu->core.M[PSW] & PSW_INTR_HALT)        // ПоП
                goto ret;
            if (cpu->core.RUU & RUU_RIGHT_INSTR) {
                ++cpu->core.PC;
            }
            cpu->core.RUU ^= RUU_RIGHT_INSTR;
            op_int_1(cpu, sim_stop_messages[r]);
            cpu->core.M[SPSW] |= SPSW_NEXT_RK;
            cpu->core.RPR |= RPR_WATCHPT_R;
            break;
        case ESS_STORE_ADDR_MATCH:
            if (cpu->core.M[PSW] & PSW_INTR_HALT)        // ПоП
                goto ret;
            if (cpu->core.RUU & RUU_RIGHT_INSTR) {
                ++cpu->core.PC;
            }
            cpu->core.RUU ^= RUU_RIGHT_INSTR;
            op_int_1(cpu, sim_stop_messages[r]);
            cpu->core.M[SPSW] |= SPSW_NEXT_RK;
            cpu->core.RPR |= RPR_WATCHPT_W;
            break;
        case ESS_OVFL:
            // Прерывание по АУ вызывает останов, если БРО=0
            // и установлен ПоП или ПоК.
            // Страница 118 ТО9.
            if (! (cpu->core.RUU & RUU_AVOST_DISABLE) && // ! БРО
                ((cpu->core.M[PSW] & PSW_INTR_HALT) ||   // ПоП
                 (cpu->core.M[PSW] & PSW_CHECK_HALT)))   // ПоК
                goto ret;
            op_int_1(cpu, sim_stop_messages[r]);
            cpu->core.RPR |= RPR_OVERFLOW|RPR_RAM_CHECK;
            break;
        case ESS_DIVZERO:
            if (! (cpu->core.RUU & RUU_AVOST_DISABLE) && // ! БРО
                ((cpu->core.M[PSW] & PSW_INTR_HALT) ||   // ПоП
                 (cpu->core.M[PSW] & PSW_CHECK_HALT)))   // ПоК
                goto ret;
            op_int_1(cpu, sim_stop_messages[r]);
            cpu->core.RPR |= RPR_DIVZERO|RPR_RAM_CHECK;
            break;
        }
        ++iintr;
    }

    if (iintr > 1) {
        return ESS_DOUBLE_INTR;
    }

    // Main instruction fetch/decode loop
    for (;;) {
        if (cpu->core.PC > BITS(15) && IS_SUPERVISOR(cpu->core.RUU)) {
            //
            // Runaway instruction execution in supervisor mode
            // warrants attention.
            //
            return ESS_RUNOUT;                 // stop simulation
        }

#if 0
        //TODO: enable breakpoints.
        if ((sim_brk_summ & SWMASK('E')) &&     // breakpoint?
            sim_brk_test(cpu->core.PC, SWMASK('E')) &&
            ! (cpu->core.RUU & RUU_RIGHT_INSTR)) {
            return ESS_IBKPT;                  // stop simulation
        }
#endif

        if (! iintr && ! (cpu->core.RUU & RUU_RIGHT_INSTR) &&
            ! (cpu->core.M[PSW] & PSW_INTR_DISABLE))
        {
            if (cpu->core.RPR) {
                // internal interrupt
                if (cpu->trace_instructions | cpu->trace_memory |
                    cpu->trace_registers | cpu->trace_fetch) {
                    fprintf(cpu->log_output, "cpu%d --- Внутреннее прерывание\n",
                        cpu->index);
                }
                op_int_2(cpu);
            }
            if (cpu->core.GRVP & cpu->core.GRM) {
                // external interrupt
                if (cpu->trace_instructions | cpu->trace_memory |
                    cpu->trace_registers | cpu->trace_fetch) {
                    fprintf(cpu->log_output, "cpu%d --- Внешнее прерывание\n",
                        cpu->index);
                }
                op_int_2(cpu);
            }
        }

        cpu_one_instr(cpu);                     // one instr
        iintr = 0;
    }
}

//
// A 250 Hz clock as per the original documentation,
// and matching the available software binaries.
// Some installations used 50 Hz with a modified OS
// for a better user time/system time ratio.
//
void cpu_activate_timer(struct ElSvsProcessor *cpu)
{
    if (cpu->trace_instructions | cpu->trace_memory |
        cpu->trace_registers | cpu->trace_fetch) {
        fprintf(cpu->log_output, "---- --- Timer\n");
    }

    cpu->core.GRVP |= GRVP_TIMER;
}
