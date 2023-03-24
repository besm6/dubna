/*
 * SVS simulator definitions
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
#ifndef __SVS_INTERNAL_H
#define __SVS_INTERNAL_H

#include <stdio.h>
#include <setjmp.h>
#include <inttypes.h>
#include <stdbool.h>

//
// Memory.
//
#define SVS_NREGS       30              // number of registers-modifiers
#define SVS_MEMSIZE     (1024 * 1024)   // memory size, words

//
// Разряды машинного слова, справа налево, начиная с 1.
//
#define BBIT(n)         (1 << (n-1))            // один бит, от 1 до 32
#define BIT40           000010000000000000LL    // 40-й бит - старший разряд мантиссы
#define BIT41           000020000000000000LL    // 41-й бит - знак
#define BIT42           000040000000000000LL    // 42-й бит - дубль-знак в мантиссе
#define BIT48           004000000000000000LL    // 48-й бит - знак порядка
#define BIT49           010000000000000000LL    // бит 49
#define BITS(n)         (~0U >> (32-n))         // маска битов n..1
#define BITS40          00017777777777777LL     // биты 40..1 - мантисса
#define BITS41          00037777777777777LL     // биты 41..1 - мантисса и знак
#define BITS42          00077777777777777LL     // биты 42..1 - мантисса и оба знака
#define BITS48          07777777777777777LL     // биты 48..1
#define ADDR(x)         ((x) & BITS(15))        // адрес слова

//
// Работа с тегами.
//
#define TAG_INSN48      035
#define TAG_NUMBER48    036
#define IS_INSN48(t)    ((t) == TAG_INSN48)
#define IS_48BIT(t)     ((t) == TAG_INSN48 || (t) == TAG_NUMBER48)

//
// Внутреннее состояние процессора.
//
struct ElSvsCoreState {
    uint32_t PC;            // счётчик команд СчАС
    uint32_t RAU, RUU;      // режим АУ, режим УУ
    uint64_t ACC, RMR;      // аккумулятор, РМР
    uint32_t M[SVS_NREGS];  // регистры-модификаторы

    //
    // 64-битные регистры RP0-RP7 - для отображения регистров приписки,
    // группами по 4 ради компактности, 12 бит на страницу.
    // TLB0-TLB31 - постраничные регистры приписки, копии RPi.
    // Обращение к памяти должно вестись через TLBi.
    //
    uint64_t RP[8];         // РП, регистры приписки страниц пользователя
    uint64_t RPS[8];        // РПС, регистры приписки страниц супервизора
    uint32_t RZ;            // РЗ, регистр защиты

    uint8_t TagR;           // регистр тега
    uint64_t RPR;           // РПР: регистр внутренних прерываний
    uint32_t GRVP;          // ГРВП: главный регистр внешних прерываний
    uint32_t GRM;           // ГРМ: главный регистр маски

    uint64_t PP, OPP;       // ПП, ОПП
    uint64_t POP, OPOP;     // ПОП, ОПОП
    uint64_t RKP;           // РКП

    uint32_t bad_addr;      // адрес, вызвавший прерывание
};

//
// Состояние одного процессора.
//
struct ElSvsProcessor {
    // Текущее состояние.
    struct ElSvsCoreState core;

    // Предыдущее состояние, для трассировки.
    struct ElSvsCoreState prev;

    int index;                  // номер процессора 0...3
    uint64_t pult[8];           // тумблерные регистры
    uint32_t RK, Aex;           // регистр команд, исполнительный адрес
    uint32_t UTLB[32];          // регистры приписки постранично, пользователя
    uint32_t STLB[32];          // регистры приписки постранично, супервизора
    jmp_buf exception;          // прерывание
    int corr_stack;             // коррекция стека при прерывании

    // Режимы трассировки.
    bool trace_instructions;    // трассировка выполняемых машинных команд
    bool trace_extracodes;      // трассировка экстракодов (кроме э75)
    bool trace_fetch;           // трассировка выборки команд из памяти
    bool trace_memory;          // трассировка чтения и записи памяти
    bool trace_exceptions;      // трассировка исключительных ситуаций
    bool trace_registers;       // трассировка регистров
    FILE *log_output;           // файл для вывода трассировки, или stdout

#if 0
    int mpd_data;           // данные для передачи в МПД
    int mpd_nbits;          // счётчик битов
#endif
};

//
// Разряды режима АУ.
//
#define RAU_NORM_DISABLE        001     // блокировка нормализации
#define RAU_ROUND_DISABLE       002     // блокировка округления
#define RAU_LOG                 004     // признак логической группы
#define RAU_MULT                010     // признак группы умножения
#define RAU_ADD                 020     // признак группы слодения
#define RAU_OVF_DISABLE         040     // блокировка переполнения

#define RAU_MODE                (RAU_LOG | RAU_MULT | RAU_ADD)
#define SET_MODE(x,m)           (((x) & ~RAU_MODE) | (m))
#define SET_LOGICAL(x)          (((x) & ~RAU_MODE) | RAU_LOG)
#define SET_MULTIPLICATIVE(x)   (((x) & ~RAU_MODE) | RAU_MULT)
#define SET_ADDITIVE(x)         (((x) & ~RAU_MODE) | RAU_ADD)
#define IS_LOGICAL(x)           (((x) & RAU_MODE) == RAU_LOG)
#define IS_MULTIPLICATIVE(x)    (((x) & (RAU_ADD | RAU_MULT)) == RAU_MULT)
#define IS_ADDITIVE(x)          ((x) & RAU_ADD)

//
// Искусственный регистр режимов УУ, в реальной машине отсутствует.
//
#define RUU_CHECK_RIGHT         000001  // ПКП - признак контроля правой половины
#define RUU_CHECK_LEFT          000002  // ПКЛ - признак контроля левой половины
#define RUU_EXTRACODE           000004  // РежЭ - режим экстракода
#define RUU_INTERRUPT           000010  // РежПр - режим прерывания
#define RUU_MOD_RK              000020  // ПрИК - модификация регистром М[16]
#define RUU_AVOST_DISABLE       000040  // БРО - блокировка режима останова
#define RUU_RIGHT_INSTR         000400  // ПрК - признак правой команды

#define IS_SUPERVISOR(x)        ((x) & (RUU_EXTRACODE | RUU_INTERRUPT))
#define SET_SUPERVISOR(x,m)     (((x) & ~(RUU_EXTRACODE | RUU_INTERRUPT)) | (m))

//
// Специальные регистры.
//
#define MOD     020     // модификатор адреса
#define PSW     021     // режимы УУ
#define SPSW    027     // упрятывание режимов УУ
#define ERET    032     // адрес возврата из экстракода
#define IRET    033     // адрес возврата из прерывания
#define IBP     034     // адрес прерывания по выполнению
#define DWP     035     // адрес прерывания по чтению/записи

//
// Регистр 021: режимы УУ.
// PSW: program status word.
//
#define PSW_MMAP_DISABLE        000001  // БлП - блокировка приписки
#define PSW_PROT_DISABLE        000002  // БлЗ - блокировка защиты
#define PSW_INTR_HALT           000004  // ПоП - признак останова при
                                        // любом внутреннем прерывании
#define PSW_CHECK_HALT          000010  // ПоК - признак останова при
                                        // прерывании по контролю
#define PSW_WRITE_WATCH         000020  // Зп(М29) - признак совпадения адреса
                                        // операнда прии записи в память
                                        // с содержанием регистра М29
#define PSW_INTR_DISABLE        002000  // БлПр - блокировка внешнего прерывания

//
// Регистр 027: сохранённые режимы УУ.
// SPSW: saved program status word.
//
#define SPSW_MMAP_DISABLE       000001  // БлП - блокировка приписки
#define SPSW_PROT_DISABLE       000002  // БлЗ - блокировка защиты
#define SPSW_EXTRACODE          000004  // РежЭ - режим экстракода
#define SPSW_INTERRUPT          000010  // РежПр - режим прерывания
#define SPSW_MOD_RK             000020  // ПрИК(РК) - на регистр РК принята
                                        // команда, которая должна быть
                                        // модифицирована регистром М[16]
#define SPSW_MOD_RR             000040  // ПрИК(РР) - на регистре РР находится
                                        // команда, выполненная с модификацией
#define SPSW_RIGHT_INSTR        000400  // ПрК - признак правой команды
#define SPSW_NEXT_RK            001000  // ГД./ДК2 - на регистр РК принята
                                        // команда, следующая после вызвавшей
                                        // прерывание
#define SPSW_INTR_DISABLE       002000  // БлПр - блокировка внешнего прерывания

//
// Кириллица Unicode.
//
#define CYRILLIC_CAPITAL_LETTER_A               0x0410
#define CYRILLIC_CAPITAL_LETTER_BE              0x0411
#define CYRILLIC_CAPITAL_LETTER_VE              0x0412
#define CYRILLIC_CAPITAL_LETTER_GHE             0x0413
#define CYRILLIC_CAPITAL_LETTER_DE              0x0414
#define CYRILLIC_CAPITAL_LETTER_IE              0x0415
#define CYRILLIC_CAPITAL_LETTER_ZHE             0x0416
#define CYRILLIC_CAPITAL_LETTER_ZE              0x0417
#define CYRILLIC_CAPITAL_LETTER_I               0x0418
#define CYRILLIC_CAPITAL_LETTER_SHORT_I         0x0419
#define CYRILLIC_CAPITAL_LETTER_KA              0x041a
#define CYRILLIC_CAPITAL_LETTER_EL              0x041b
#define CYRILLIC_CAPITAL_LETTER_EM              0x041c
#define CYRILLIC_CAPITAL_LETTER_EN              0x041d
#define CYRILLIC_CAPITAL_LETTER_O               0x041e
#define CYRILLIC_CAPITAL_LETTER_PE              0x041f
#define CYRILLIC_CAPITAL_LETTER_ER              0x0420
#define CYRILLIC_CAPITAL_LETTER_ES              0x0421
#define CYRILLIC_CAPITAL_LETTER_TE              0x0422
#define CYRILLIC_CAPITAL_LETTER_U               0x0423
#define CYRILLIC_CAPITAL_LETTER_EF              0x0424
#define CYRILLIC_CAPITAL_LETTER_HA              0x0425
#define CYRILLIC_CAPITAL_LETTER_TSE             0x0426
#define CYRILLIC_CAPITAL_LETTER_CHE             0x0427
#define CYRILLIC_CAPITAL_LETTER_SHA             0x0428
#define CYRILLIC_CAPITAL_LETTER_SHCHA           0x0429
#define CYRILLIC_CAPITAL_LETTER_HARD_SIGN       0x042a
#define CYRILLIC_CAPITAL_LETTER_YERU            0x042b
#define CYRILLIC_CAPITAL_LETTER_SOFT_SIGN       0x042c
#define CYRILLIC_CAPITAL_LETTER_E               0x042d
#define CYRILLIC_CAPITAL_LETTER_YU              0x042e
#define CYRILLIC_CAPITAL_LETTER_YA              0x042f
#define CYRILLIC_SMALL_LETTER_A                 0x0430
#define CYRILLIC_SMALL_LETTER_BE                0x0431
#define CYRILLIC_SMALL_LETTER_VE                0x0432
#define CYRILLIC_SMALL_LETTER_GHE               0x0433
#define CYRILLIC_SMALL_LETTER_DE                0x0434
#define CYRILLIC_SMALL_LETTER_IE                0x0435
#define CYRILLIC_SMALL_LETTER_ZHE               0x0436
#define CYRILLIC_SMALL_LETTER_ZE                0x0437
#define CYRILLIC_SMALL_LETTER_I                 0x0438
#define CYRILLIC_SMALL_LETTER_SHORT_I           0x0439
#define CYRILLIC_SMALL_LETTER_KA                0x043a
#define CYRILLIC_SMALL_LETTER_EL                0x043b
#define CYRILLIC_SMALL_LETTER_EM                0x043c
#define CYRILLIC_SMALL_LETTER_EN                0x043d
#define CYRILLIC_SMALL_LETTER_O                 0x043e
#define CYRILLIC_SMALL_LETTER_PE                0x043f
#define CYRILLIC_SMALL_LETTER_ER                0x0440
#define CYRILLIC_SMALL_LETTER_ES                0x0441
#define CYRILLIC_SMALL_LETTER_TE                0x0442
#define CYRILLIC_SMALL_LETTER_U                 0x0443
#define CYRILLIC_SMALL_LETTER_EF                0x0444
#define CYRILLIC_SMALL_LETTER_HA                0x0445
#define CYRILLIC_SMALL_LETTER_TSE               0x0446
#define CYRILLIC_SMALL_LETTER_CHE               0x0447
#define CYRILLIC_SMALL_LETTER_SHA               0x0448
#define CYRILLIC_SMALL_LETTER_SHCHA             0x0449
#define CYRILLIC_SMALL_LETTER_HARD_SIGN         0x044a
#define CYRILLIC_SMALL_LETTER_YERU              0x044b
#define CYRILLIC_SMALL_LETTER_SOFT_SIGN         0x044c
#define CYRILLIC_SMALL_LETTER_E                 0x044d
#define CYRILLIC_SMALL_LETTER_YU                0x044e
#define CYRILLIC_SMALL_LETTER_YA                0x044f

//
// Процедуры работы с памятью
//
void mmu_store(struct ElSvsProcessor *cpu, int addr, uint64_t word);
void mmu_store64(struct ElSvsProcessor *cpu, int addr, uint64_t word);
uint64_t mmu_load(struct ElSvsProcessor *cpu, int addr);
uint64_t mmu_load64(struct ElSvsProcessor *cpu, int addr, int tag_check);
uint64_t mmu_fetch(struct ElSvsProcessor *cpu, int addr, int *paddrp);
void mmu_set_rp(struct ElSvsProcessor *cpu, int idx, uint64_t word, int supervisor);
void mmu_setup(struct ElSvsProcessor *cpu);
void mmu_set_protection(struct ElSvsProcessor *cpu, int idx, uint64_t word);

//
// Отладочная выдача.
//
void svs_fprint_cmd(FILE *of, uint32_t cmd);
void svs_fprint_insn(FILE *of, uint32_t insn);
void svs_trace_opcode(struct ElSvsProcessor *cpu, int paddr);
void svs_trace_registers(struct ElSvsProcessor *cpu);
void svs_fprint_48bits(FILE *of, uint64_t value);

//
// Арифметика.
//
double svs_to_ieee(uint64_t word);
void svs_add(struct ElSvsProcessor *cpu, uint64_t val, int negate_acc, int negate_val);
void svs_divide(struct ElSvsProcessor *cpu, uint64_t val);
void svs_multiply(struct ElSvsProcessor *cpu, uint64_t val);
void svs_change_sign(struct ElSvsProcessor *cpu, int sign);
void svs_add_exponent(struct ElSvsProcessor *cpu, int val);
int svs_highest_bit(uint64_t val);
void svs_shift(struct ElSvsProcessor *cpu, int toright);
int svs_count_ones(uint64_t word);
uint64_t svs_pack(uint64_t val, uint64_t mask);
uint64_t svs_unpack(uint64_t val, uint64_t mask);

//
// Процессор ввода-вывода.
//
//void iom_reset(int index);
//void iom_request(int index);

//
// МПД.
//
//void mpd_reset(struct ElSvsProcessor *cpu);
//void mpd_send_nibble(struct ElSvsProcessor *cpu, int data);
//void mpd_receive_update(struct ElSvsProcessor *cpu);

//
// Разряды главного регистра прерываний (ГРП)
//
// Внешние:
#define RPR_WATCHDOG    00000000000002000LL // 11
//
// Внутренние:
#define RPR_DIVZERO     00000000034000000LL // 23-21
#define RPR_OVERFLOW    00000000014000000LL // 22-21
#define RPR_CHECK       00000000004000000LL // 21
#define RPR_OPRND_PROT  00000000002000000LL // 20
#define RPR_WATCHPT_W   00000000000200000LL // 17
#define RPR_WATCHPT_R   00000000000100000LL // 16
#define RPR_INSN_CHECK  00000000000040000LL // 15
#define RPR_INSN_PROT   00000000000020000LL // 14
#define RPR_ILL_INSN    00000000000010000LL // 13
#define RPR_BREAKPOINT  00000000000004000LL // 12
#define RPR_PAGE_MASK   00000000000000760LL // 9-5
#define RPR_RAM_CHECK   00000000000000010LL // 4
#define RPR_BLOCK_MASK  00000000000000007LL // 3-1

#define RPR_SET_BLOCK(x,m)  (((x) & ~RPR_BLOCK_MASK) | ((m) & RPR_BLOCK_MASK))
#define RPR_SET_PAGE(x,m)   (((x) & ~RPR_PAGE_MASK) | (((m)<<4) & RPR_PAGE_MASK))

//
// Разряды регистра внешних прерываний РВП
//
#define GRVP_PROGRAM    0400LL
#define GRVP_REQUEST    0200LL
#define GRVP_RESPONSE   0100LL
#define GRVP_IOM_FAIL   0040LL
#define GRVP_RAM_FAIL   0020LL
#define GRVP_TIMER      0010LL
#define GRVP_INTR_IOM   0004LL
#define GRVP_MULTI      0002LL
#define GRVP_PANEL_REQ  0001LL

//
// Разряды регистров РКП, ПП, ОПП, ПОП, ОПОП.
//
#define CONF_IOM_RESET  (1LL << 47)     // бит 48: Сброс ПВВ
#define CONF_IOM1       (1LL << 45)     // бит 46: ПВВ 1
#define CONF_IOM2       (1LL << 44)     // бит 45: ПВВ 2
#define CONF_IOM3       (1LL << 43)     // бит 44: ПВВ 3
#define CONF_IOM4       (1LL << 42)     // бит 43: ПВВ 4
#define CONF_IOM_MASK   (0xfLL << 42)   // биты 43-46: биты ПВВ
#define CONF_CPU_MASK   (0xfLL << 38)   // биты 39-42: биты процесоров СВС
#define CONF_DATA_MASK  (0xfLL << 34)   // биты 35-38: данные МПД
#define CONF_MR         (1LL << 33)     // бит 34: приём МПД
#define CONF_MT         (1LL << 32)     // бит 33: передача МПД

#define CONF_GET_DATA(x)    (((x) >> 34) & 0xf)
#define CONF_SET_DATA(r,x)  (((r) & ~CONF_DATA_MASK) | (((x) & 0xfLL) << 34))

#endif // __SVS_INTERNAL_H
