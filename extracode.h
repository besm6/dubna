//
// Declarations for extracodes.
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
#ifndef DUBNA_EXTRACODE_H
#define DUBNA_EXTRACODE_H

//
// Экстракод 070: обмен с внешней памятью.
//
// Информационное слово по исполнительному адресу задаёт параметры обмена.
// Если исполнительный адрес равен нулю - информационное слово находится на сумматоре.
//
// При обмене с магнитными барабанами:
//  * Разряд 48 = 0 - обмен между листом ОП и трактом МБ;
//              = 1 - секторный обмен между абзацем листа и сектором МБ;
//  * Разряд 40 = 0 - запись из ОП на МБ;
//              = 1 - чтение с МБ в ОП;
//  * Разряд 39     - признак физ.обмена, перенаправляемого на первый заказанный диск,
//                    с учётом смещения относительно номера ФИЗ МБ в паспорте;
//  * Разряд 36     - для секторного обмена,
//                    номер тракта в битах 7-3, номер сектора в битах 2-1;
//  * Разряды 35-31 - номер листа;
//  * Разряды 26-25 - номер абзаца в листе;
//  * Разряды 18-13 - логический номер МБ;
//  * Разряды 8-7   - номер сектора в тракте;
//  * Разряды 5-1   - номер тракта.
//
// При обмене с магнитными дисками:
//  * Разряд 40 = 0 - запись листа ОП в зону МД;
//              = 1 - чтение зоны МД в лист ОП;
//  * Разряды 35-31 - номер листа;
//  * Разряды 18-13 - логический номер МД;
//  * Разряды 12-1  - номер зоны.
//
#pragma pack(push, 1)
union E70_Info {
    Word word;
    struct {
        unsigned zone     : 12;     // Zone number
        unsigned unit     : 6;      // Disk unit number
        unsigned _1       : 12;     // ---
        unsigned page     : 5;      // Memory page number
        unsigned _2       : 4;      // ---
        unsigned read_op  : 1;      // Operation: 1=Read, 0=Write
        unsigned _3       : 8;      // ---
    } disk;
    struct {
        unsigned tract    : 5;      // Tract number
        unsigned _1       : 1;      // ---
        unsigned sector   : 2;      // Sector number in the tract
        unsigned _2       : 4;      // ---
        unsigned unit     : 6;      // Disk unit number
        unsigned _3       : 6;      // ---
        unsigned paragraph: 2;      // Paragraph number in the memory page
        unsigned _4       : 4;      // ---
        unsigned page     : 5;      // Memory page number
        unsigned raw_sect : 1;      // Raw sector number instead of tract/sector
        unsigned _5       : 2;      // ---
        unsigned phys_io  : 1;      // Redirection to physical disk
        unsigned read_op  : 1;      // Operation: 1=Read, 0=Write
        unsigned _6       : 7;      // ---
        unsigned sect_io  : 1;      // When 1 - transfer only one sector
    } drum;
};
#pragma pack(pop)

#endif // DUBNA_EXTRACODE_H
