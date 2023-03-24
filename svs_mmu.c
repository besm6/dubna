/*
 * SVS memory management unit.
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
#include "el_master_api.h"
#include "el_svs_api.h"
#include "el_svs_internal.h"

static void mmu_protection_check(struct ElSvsProcessor *cpu, int vaddr)
{
    // Защита блокируется в режиме супервизора для физических (!) адресов 1-7 (ТО-8) - WTF?
    int tmp_prot_disabled = (cpu->core.M[PSW] & PSW_PROT_DISABLE) ||
        (IS_SUPERVISOR(cpu->core.RUU) && (cpu->core.M[PSW] & PSW_MMAP_DISABLE) && vaddr < 010);

    // Защита не заблокирована, а лист закрыт
    if (! tmp_prot_disabled && (cpu->core.RZ & (1 << (vaddr >> 10)))) {
        cpu->core.bad_addr = vaddr >> 10;
        if (cpu->trace_exceptions)
            printf("--- (%05o) защита числа", vaddr);
        longjmp(cpu->exception, ESS_OPERAND_PROT);
    }
}

//
// Трансляция виртуального адреса в физический.
//
static int va_to_pa(struct ElSvsProcessor *cpu, int vaddr)
{
    int paddr;

    if (cpu->core.M[PSW] & PSW_MMAP_DISABLE) {
        // Приписка отключена.
        paddr = vaddr;
    } else {
        // Приписка работает.
        int vpage    = vaddr >> 10;
        int offset   = vaddr & BITS(10);
        int physpage = IS_SUPERVISOR(cpu->core.RUU) ?
                       cpu->STLB[vpage] : cpu->UTLB[vpage];

        paddr = (physpage << 10) | offset;
    }
    return paddr;
}

//
// Запись слова и тега в память по виртуальному адресу.
// Возвращает физический адрес слова.
//
static int mmu_store_with_tag(struct ElSvsProcessor *cpu, int vaddr, uint64_t val64, uint8_t t)
{
    vaddr &= BITS(15);
    if (vaddr == 0)
        return 0;

    mmu_protection_check(cpu, vaddr);

    // Различаем адреса с припиской и без
    if (cpu->core.M[PSW] & PSW_MMAP_DISABLE) {
        // Приписка отключена.
        if (vaddr < 010) {
            // Игнорируем запись в тумблерные регистры.
            if (cpu->trace_instructions | cpu->trace_memory | cpu->trace_registers) {
                fprintf(cpu->log_output, "cpu%d --- Ignore write to pult register %d\n",
                    cpu->index, vaddr);
            }
            return 0;
        }
    } else {
        // Приписка работает.
        // ЗПСЧ: ЗП
        if (cpu->core.M[DWP] == vaddr && (cpu->core.M[PSW] & PSW_WRITE_WATCH))
            longjmp(cpu->exception, ESS_STORE_ADDR_MATCH);
#if 0
        // Точка останова по записи.
        if (sim_brk_summ & SWMASK('W') &&
            sim_brk_test(vaddr, SWMASK('W')))
            longjmp(cpu->exception, ESS_WWATCH);
#endif
    }

    // Вычисляем физический адрес.
    int paddr = va_to_pa(cpu, vaddr);

    // Пишем в память.
    elMasterRamWordWrite(paddr, t, val64);

    return paddr;
}

//
// Запись 48-битного слова в память.
//
void mmu_store(struct ElSvsProcessor *cpu, int vaddr, uint64_t val)
{
    // Вычисляем тег.
    // Если ПКП=0 и ПКЛ=0, то тег 35 (команда),
    // иначе тег 36 (данные).
    uint8_t t = (cpu->core.RUU & (RUU_CHECK_RIGHT | RUU_CHECK_LEFT)) ?
        TAG_NUMBER48 : TAG_INSN48;

    int paddr = mmu_store_with_tag(cpu, vaddr, val << 16, t);

    if (paddr != 0 && cpu->trace_memory) {
        fprintf(cpu->log_output, "cpu%d       Memory Write [%05o %07o] = %02o:",
            cpu->index, vaddr, paddr, t);
        svs_fprint_48bits(cpu->log_output, val);
        fprintf(cpu->log_output, "\n");
    }
}

//
// Запись 64-битного слова в память.
//
void mmu_store64(struct ElSvsProcessor *cpu, int vaddr, uint64_t val64)
{
    int paddr = mmu_store_with_tag(cpu, vaddr, val64, cpu->core.TagR);

    if (paddr != 0 && cpu->trace_memory) {
        fprintf(cpu->log_output, "cpu%d       Memory Write [%05o %07o] = %02o:",
            cpu->index, vaddr, paddr, cpu->core.TagR);
        fprintf(cpu->log_output, "%04o %04o %04o %04o:%02o %04o\n",
            (int) (val64 >> 52) & 07777,
            (int) (val64 >> 40) & 07777,
            (int) (val64 >> 28) & 07777,
            (int) (val64 >> 16) & 07777,
            (int) (val64 >> 12) & 017,
            (int) val64 & 07777);
    }
}

//
// Чтение операнда и тега из памяти по виртуальному адресу.
// Возвращает физический адрес слова.
//
static int mmu_load_with_tag(struct ElSvsProcessor *cpu, int vaddr, uint64_t *val64, uint8_t *t)
{
    vaddr &= BITS(15);
    if (vaddr == 0) {
        *val64 = 0;
        *t = 0;
        return 0;
    }

    mmu_protection_check(cpu, vaddr);

    // Различаем адреса с припиской и без
    if (cpu->core.M[PSW] & PSW_MMAP_DISABLE) {
        // Приписка отключена.
    } else {
        // Приписка работает.
        // ЗПСЧ: СЧ
        if (cpu->core.M[DWP] == vaddr && !(cpu->core.M[PSW] & PSW_WRITE_WATCH))
            longjmp(cpu->exception, ESS_LOAD_ADDR_MATCH);
#if 0
        // Точка останова по считыванию.
        if (sim_brk_summ & SWMASK('R') &&
            sim_brk_test(vaddr, SWMASK('R')))
            longjmp(cpu->exception, ESS_RWATCH);
#endif
    }

    // Вычисляем физический адрес слова
    int paddr = va_to_pa(cpu, vaddr);

    if (paddr >= 010) {
        // Из памяти
        elMasterRamWordRead(paddr, t, val64);
    } else {
        // С тумблерных регистров
        *val64 = cpu->pult[paddr] << 16;
        *t = TAG_INSN48;
    }
    return paddr;
}

//
// Чтение 64-битного операнда.
// Тег попадает в регистр тега.
//
uint64_t mmu_load64(struct ElSvsProcessor *cpu, int vaddr, int tag_check)
{
    uint64_t val64;
    uint8_t t;
    int paddr = mmu_load_with_tag(cpu, vaddr, &val64, &t);

    if (paddr != 0 && cpu->trace_memory) {
        if (paddr < 010)
            fprintf(cpu->log_output, "cpu%d       Read  TR%o = ", cpu->index, paddr);
        else
            fprintf(cpu->log_output, "cpu%d       Memory Read [%05o %07o] = %02o:",
                cpu->index, vaddr, paddr, t);
        fprintf(cpu->log_output, "%04o %04o %04o %04o:%02o %04o\n",
            (int) (val64 >> 52) & 07777,
            (int) (val64 >> 40) & 07777,
            (int) (val64 >> 28) & 07777,
            (int) (val64 >> 16) & 07777,
            (int) (val64 >> 12) & 017,
            (int) val64 & 07777);
    }

    // Прерывание (контроль числа), если попалось 48-битное слово.
    if (tag_check && IS_48BIT(t) /*&& (mmu_unit.flags & CHECK_ENB)*/) {
        cpu->core.bad_addr = paddr & 7;
        printf("--- (%05o) контроль числа", paddr);
        longjmp(cpu->exception, ESS_RAM_CHECK);
    }

    cpu->core.TagR = t;
    return val64;
}

//
// Чтение 48-битного операнда.
//
uint64_t mmu_load(struct ElSvsProcessor *cpu, int vaddr)
{
    uint64_t val;
    uint8_t t;
    int paddr = mmu_load_with_tag(cpu, vaddr, &val, &t);

    val >>= 16;
    if (paddr != 0 && cpu->trace_memory) {
        if (paddr < 010)
            fprintf(cpu->log_output, "cpu%d       Read  TR%o = ", cpu->index, paddr);
        else
            fprintf(cpu->log_output, "cpu%d       Memory Read [%05o %07o] = %02o:",
                cpu->index, vaddr, paddr, t);
        svs_fprint_48bits(cpu->log_output, val);
        fprintf(cpu->log_output, "\n");
    }

    // Прерывание (контроль числа), если попалось 64-битное слово.
    // На тумблерных регистрах контроля числа не бывает.
    if (paddr >= 010 && ! IS_48BIT(t) /*&& (mmu_unit.flags & CHECK_ENB)*/) {
        cpu->core.bad_addr = paddr & 7;
        printf("--- (%05o) контроль числа", paddr);
        longjmp(cpu->exception, ESS_RAM_CHECK);
    }

    // Тег не запоминаем.
    return val & BITS48;
}

static void mmu_fetch_check(struct ElSvsProcessor *cpu, int vaddr)
{
    // В режиме супервизора защиты нет
    if (! IS_SUPERVISOR(cpu->core.RUU)) {
        int page = cpu->UTLB[vaddr >> 10];
        //
        // Для команд в режиме пользователя признак защиты -
        // 0 в регистре приписки.
        //
        if (page == 0) {
            cpu->core.bad_addr = vaddr >> 10;
            if (cpu->trace_exceptions)
                printf("--- (%05o) защита команды", vaddr);
            longjmp(cpu->exception, ESS_INSN_PROT);
        }
    }
}

//
// Выборка команды
//
uint64_t mmu_fetch(struct ElSvsProcessor *cpu, int vaddr, int *paddrp)
{
    uint64_t val;
    uint8_t t;

    if (vaddr == 0) {
        if (cpu->trace_exceptions)
            printf("--- передача управления на 0");
        longjmp(cpu->exception, ESS_INSN_CHECK);
    }

    mmu_fetch_check(cpu, vaddr);

    // КРА
    if (cpu->core.M[IBP] == vaddr && ! IS_SUPERVISOR(cpu->core.RUU))
        longjmp(cpu->exception, ESS_INSN_ADDR_MATCH);

    // Вычисляем физический адрес слова
    int paddr = IS_SUPERVISOR(cpu->core.RUU) ? vaddr : va_to_pa(cpu, vaddr);

    if (paddr >= 010) {
        // Из памяти
        uint64_t val64;
        elMasterRamWordRead(paddr, &t, &val64);
        val = val64 >> 16;
    } else {
        // from switch regs
        val = cpu->pult[paddr];
        t = TAG_INSN48;
    }

    if (cpu->trace_fetch && !(cpu->core.RUU & RUU_RIGHT_INSTR)) {
        // Print the fetch information.
        fprintf(cpu->log_output, "cpu%d       Fetch [%05o %07o] = %o:",
            cpu->index, vaddr, paddr, t);
        svs_fprint_insn(cpu->log_output, (val >> 24) & BITS(24));
        svs_fprint_insn(cpu->log_output, val & BITS(24));
        fprintf(cpu->log_output, "\n");
    }

    // Прерывание (контроль команды), если попалась не 48-битная команда.
    // Тумблерные регистры только с командной сверткой.
    if (paddr >= 010 && ! IS_INSN48(t)) {
        printf("--- (%05o) контроль команды", vaddr);
        longjmp(cpu->exception, ESS_INSN_CHECK);
    }

    *paddrp = paddr;
    return val & BITS48;
}

void mmu_set_rp(struct ElSvsProcessor *cpu, int idx, uint64_t val, int supervisor)
{
    uint32_t p0, p1, p2, p3;
    const uint32_t mask = (SVS_MEMSIZE >> 10) - 1;

    // Младшие 5 разрядов 4-х регистров приписки упакованы
    // по 5 в 1-20 рр, 6-е разряды - в 29-32 рр, 7-е разряды - в 33-36 рр и т.п.
    //
    p0 = (val       & 037) | (((val>>28) & 1) << 5) | (((val>>32) & 1) << 6) | (((val>>36) &  1) << 7) | (((val>>40) & 1) << 8) | (((val>>44) & 1) << 9);
    p1 = ((val>>5)  & 037) | (((val>>29) & 1) << 5) | (((val>>33) & 1) << 6) | (((val>>37) &  1) << 7) | (((val>>41) & 1) << 8) | (((val>>45) & 1) << 9);
    p2 = ((val>>10) & 037) | (((val>>30) & 1) << 5) | (((val>>34) & 1) << 6) | (((val>>38) &  1) << 7) | (((val>>42) & 1) << 8) | (((val>>46) & 1) << 9);
    p3 = ((val>>15) & 037) | (((val>>31) & 1) << 5) | (((val>>35) & 1) << 6) | (((val>>39) &  1) << 7) | (((val>>43) & 1) << 8) | (((val>>47) & 1) << 9);

    p0 &= mask;
    p1 &= mask;
    p2 &= mask;
    p3 &= mask;

    if (supervisor) {
        cpu->core.RPS[idx] = p0 | p1 << 12 | (uint64_t)p2 << 24 | (uint64_t)p3 << 36;
        cpu->STLB[idx*4] = p0;
        cpu->STLB[idx*4+1] = p1;
        cpu->STLB[idx*4+2] = p2;
        cpu->STLB[idx*4+3] = p3;
    } else {
        cpu->core.RP[idx] = p0 | p1 << 12 | (uint64_t)p2 << 24 | (uint64_t)p3 << 36;
        cpu->UTLB[idx*4] = p0;
        cpu->UTLB[idx*4+1] = p1;
        cpu->UTLB[idx*4+2] = p2;
        cpu->UTLB[idx*4+3] = p3;
    }
}

void mmu_setup(struct ElSvsProcessor *cpu)
{
    const uint32_t mask = (SVS_MEMSIZE >> 10) - 1;
    int i;

    // Перепись РПi в TLBj.
    for (i=0; i<8; ++i) {
        cpu->UTLB[i*4] = cpu->core.RP[i] & mask;
        cpu->UTLB[i*4+1] = cpu->core.RP[i] >> 12 & mask;
        cpu->UTLB[i*4+2] = cpu->core.RP[i] >> 24 & mask;
        cpu->UTLB[i*4+3] = cpu->core.RP[i] >> 36 & mask;
        cpu->STLB[i*4] = cpu->core.RPS[i] & mask;
        cpu->STLB[i*4+1] = cpu->core.RPS[i] >> 12 & mask;
        cpu->STLB[i*4+2] = cpu->core.RPS[i] >> 24 & mask;
        cpu->STLB[i*4+3] = cpu->core.RPS[i] >> 36 & mask;
    }
}

void mmu_set_protection(struct ElSvsProcessor *cpu, int idx, uint64_t val)
{
    // Разряды сумматора, записываемые в регистр защиты - 21-28
    int mask = 0xff << (idx * 8);

    val = ((val >> 20) & 0xff) << (idx * 8);
    cpu->core.RZ = (uint32_t)((cpu->core.RZ & ~mask) | val);
}
