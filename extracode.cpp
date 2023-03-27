//
// Execute extracodes.
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
// Execute extracode.
//
void Processor::extracode(unsigned opcode)
{
    switch (opcode) {
    case 070:
        // Disk or drum i/o.
        e70();
        break;

    case 074:
        // Finish the job.
        longjmp(exception, ESS_HALT);
        break;

    default:
        longjmp(exception, ESS_UNIMPLEMENTED);
    }
}

//
// Экстракод 070: обмен с внешней памятью.
//
// Информационное слово по исполнительному адресу задаёт параметры обмена.
// Если исполнительный адрес равен нулю - информационное слово находится на сумматоре.
//
// При обмене с магнитными барабанами:
//  * Разряд 48 = 0 - обмен между листом ОП и трактом МБ;
//              = 1 - обмен между абзацем листа и сектором МБ;
//  * Разряд 40 = 0 - запись из ОП на МБ;
//              = 1 - чтение с МБ в ОП;
//  * Разряды 35-31 - номер листа;
//  * Разряды 26-25 - номер абзаца листа;
//  * Разряды 18-13 - логический номер МБ;
//  * Разряды 8-7   - номер сектора тракта;
//  * Разряды 5-1   - номер тракта.
//
// При обмене с магнитными дисками:
//  * Разряд 40 = 0 - запись листа ОП в зону МД;
//              = 1 - чтение зоны МД в лист ОП;
//  * Разряды 35-31 - номер листа;
//  * Разряды 18-13 - логический номер МД;
//  * Разряды 12-1  - номер зоны.
//
void Processor::e70()
{
    //TODO
    longjmp(exception, ESS_UNIMPLEMENTED);
#if 0
    ushort addr   = reg[016], u, zone;
    ushort sector = 0;
    uinstr_t uil, uir;
    int r;
    static uchar buf[6144];
    static char cvbuf[1024];

    LOAD(acc, addr);
    unpack(addr);
    uil = uicore[addr][0];
    uir = uicore[addr][1];
    cwadj(&uil);
    cwadj(&uir);
    addr = (uil.i_addr & 03700) << 4;
    u    = uir.i_opcode & 077;
    if ((u < 030) || (u >= 070))
        zone = uir.i_addr & 037;
    else
        zone = uir.i_addr & 07777;
    if (uil.i_opcode & 4) { /* физобмен */
        zone += (u - (phdrum & 077)) * 040;
        u = phdrum >> 8;
    }
    if (!disks[u].diskh) {
        if (!disks[u].diskno) {
            return E_CWERR;
        } else {
            if (!(disks[u].diskh = disk_open(disks[u].diskno, disks[u].mode)))
                return E_INT;
        }
    }

    if (uil.i_reg & 8) {
        /* согласно ВЗУ и ХЛАМу, 36-й разряд означает, что номер "зоны"
         * есть не номер тракта, а номер сектора (обмен по КУС).
         */
        if (uil.i_addr & 04000) {
            zone   = uir.i_addr & 0177;
            sector = zone & 3;
            zone >>= 2;
        } else {
            sector = (uir.i_addr >> 6) & 3;
        }
        r = disk_readi(disks[u].diskh, (zone + disks[u].offset) & 0xfff, (char *)buf, cvbuf, NULL,
                       DISK_MODE_QUIET);
        if (!(uil.i_opcode & 010)) {
            memcpy(buf + sector * 256 * 6, core + addr + (uil.i_addr & 3) * 256, 256 * 6);
            memcpy(cvbuf + sector * 256, convol + addr + (uil.i_addr & 3) * 256, 256);
            r = disk_writei(disks[u].diskh, (zone + disks[u].offset) & 0xfff, (char *)buf, cvbuf,
                            NULL, DISK_MODE_QUIET);
        }
    } else if (uil.i_opcode & 010) {
        char cwords[48];
        int iomode = DISK_MODE_QUIET;
        if (uil.i_addr & 04000 && disks[u].diskno >= 2048) {
            /* листовой обмен с диском по КУС - физический номер зоны */
            iomode = DISK_MODE_PHYS;
        }
        r = disk_readi(disks[u].diskh, (zone + disks[u].offset) & 0xfff, (char *)(core + addr),
                       (char *)convol + addr, cwords, iomode);

        if (uil.i_opcode & 1 && disks[u].diskno < 2048) {
            /* check words requested for tape */
            put_check_words(u, zone, addr, 0 != (uir.i_opcode & 0200));
        } else if (uil.i_addr & 04000 && disks[u].diskno >= 2048) {
            /* copy disk check words to requested page */
            /* what should happen to the zone data? */
            memcpy((char *)(core + addr), cwords, 48);
        }
    } else {
        r = disk_writei(disks[u].diskh, (zone + disks[u].offset) & 0xfff, (char *)(core + addr),
                        (char *)convol + addr, NULL, DISK_MODE_QUIET);
    }

    if (disks[u].diskno) {
        disk_close(disks[u].diskh);
        disks[u].diskh = 0;
    }
    if (r != DISK_IO_OK)
        return E_DISKERR;

    if (uil.i_reg & 8 && uil.i_opcode & 010) {
        memcpy(core + addr + (uil.i_addr & 3) * 256, buf + sector * 256 * 6, 256 * 6);
        memcpy(convol + addr + (uil.i_addr & 3) * 256, cvbuf + sector * 256, 256);
        for (u = addr + (uil.i_addr & 3) * 256; u < addr + (uil.i_addr & 3) * 256 + 256; ++u)
            cflags[u] &= ~C_UNPACKED;
    } else if (uil.i_opcode & 010) {
        for (u = addr; u < addr + 1024; ++u)
            cflags[u] &= ~C_UNPACKED;
        if (uil.i_opcode & 1)
            for (u = 010; u < 020; ++u)
                cflags[u] &= ~C_UNPACKED;
    }
    return E_SUCCESS;
#endif
}
