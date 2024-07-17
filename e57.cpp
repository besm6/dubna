//
// Extracode 057 - floor() function, calcomp plotter, file i/o.
//
// Copyright (c) 2023-2024 Serge Vakulenko
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
// Extracode 057.
//
void Processor::e57()
{
    if (RK == 06057'0400) {
        // 12,*57,400B - unknown, for CERN routine ELPAHY (test d302).
        return;
    }
    auto addr = core.M[016];
    switch (addr) {
    case 0:
        core.ACC = besm6_floor(core.ACC);
        return;
    case 2:
        // Output to Calcomp plotter.
        machine.plotter.calcomp_putch(core.ACC);
        core.ACC = 0;
        return;
    case 3:
    case 7:
        // Delay the task, presumably waiting for tape to be installed by operator.
        throw Exception("Task paused waiting for tape");
    case 1:
    case 4:
        // CDC tape control by Gusev.
        core.ACC = 0;
        return;
    case 5:
        // Unknown, for Forex.
        core.ACC = 0;
        return;
    case 077777:
        // Manage discs and files.
        e57_file();
        return;
    default:
        if (addr >= 010) {
            // Manage tapes.
            e57_tape();
            return;
        }
        throw Exception("Unimplemented extracode *57 " + to_octal(addr));
    }
}

//
// Extracode 057 10...7777: mount tapes.
//
void Processor::e57_tape()
{
    // Modes of *57 in address field.
    enum {
        NOTFOUND = 010,   // печать 'нет магнитной ленты'
        BUSY     = 020,   // печать 'занят магнитофон'
        READY    = 040,   // печать 'нe гoтов магнитофон'
        WRITE    = 0100,  // печать 'запись'
        READ     = 0200,  // печать 'чтение'
        NODIAG   = 0400,  // блокировка любой печати
        BYNAME   = 01000, // поиск только по имени (без N бобины)
        ASSIGN   = 02000, // поиск c захватом ленты (мат.номер ленты - в 13 регистре)
        RELEASE  = 04000, // отказ от cвоиx лент, заданныx на сумматоре битовой шкалой:
    };                    // 48 разряд - лента 30 для мат.задач
    auto addr = core.M[016];

    if (addr & ASSIGN) {
        //
        // Mount tape (by name and number) on given disk number.
        // Disk number is provided in register #13.
        //
        bool write_permit = addr & WRITE;
        machine.disk_mount(core.M[015], core.ACC, write_permit);
        core.ACC = core.M[015];
        return;
    }

    if (addr & RELEASE) {
        //
        // Release tapes according to bitmask on accumulator.
        //
        if (addr & READY) {
            // Flush output buffers?
        } else {
            machine.disk_release(core.ACC);
        }
        core.ACC = 0;
        return;
    }

    //
    // Find mounted tape (by name and number).
    // Return disk number in range 030-077.
    //
    core.ACC = machine.disk_find(core.ACC);
}

//
// Extracode 057 77777: mount files.
//
void Processor::e57_file()
{
    // Use control word at accumulator.
    E57_File_Info info;
    info.word = core.ACC;

    machine.trace_e57_file(info);

    if ((info.word & E57_File_Info::KEY_BITMASK) != E57_File_Info::KEY_VALUE) {
        throw Exception("Wrong access key in *57 77777");
    }
    switch (info.field.op) {
    case E57_File_Info::VOLUME_OPEN:
        // Disk name is located at addr+1.
        // Accept any disk name for now.
        core.ACC = 0;
//
// заказ обычных файлов
//
// формируем стек вот с такой структурой:
//     1. признак монополии на мд               0000 0000 0000 0000
//     2. имя пакета                            4657 5700 0000 0001
//     3. запрос координат одного файла
//     4. запрос координат след.файла
//        .........................
//        запрос координат последнего файла
//        бит 48                                4000 0000 0000 0000
//
// запрос координат файла выглядит так:
//     1. имя хозяина файла                     0000 0000 0000 0000
//     2. имя файла                             2044 0522 1002 0040
//     3. поле ответа (+бит 30, если запись)    0000 0040 0000 0000
//     4. бит 48 + мат.номер файла в 1:6 р.     4000 0000 0000 0041
//
        break;
    case E57_File_Info::VOLUME_RELEASE:
        throw Exception("Extracode *57 77777: operation 'Release Volume' not supported yet");
        break;
    case E57_File_Info::FILE_SEARCH:
        // File name is located at addr.
        //throw Exception("Extracode *57 77777: operation 'Search File' not supported yet");
        // Accept for now.
        core.ACC = 0;
//
// в поле ответа для каждого файла
// будут его координаты на дп
// а также признаки всяких бяк:
//     вiт48 - плохое обращение
//     вiт47 - файла нет на дп
//     вiт46 - нет доступа к файлу
//
        break;
    case E57_File_Info::FILE_OPEN:
        // File name is located at addr.
        //throw Exception("Extracode *57 77777: operation 'Open File' not supported yet");
        // Accept for now.
        core.ACC = 0;
//
// формируется стек для похода на заказ файлов (т=3)
//     1. имя дп
//     2. координаты первого файла (берутся из поля ответа предыдущего стека + мат.номер в 37:42 р.)
//        ...........................
//        координаты последнего файла
//        нулевая ячейка
//
// в ячейках по файлам:
//     бит 43 - нет пакета
//     бит 44 - занят мат.номер
//     бит 45 - занят файл
//     бит 46 - нет места в fата
//
        break;
    case E57_File_Info::SCRATCH_OPEN:
        throw Exception("Extracode *57 77777: operation 'Open Scratch' not supported yet");
//
// структура инф.поля такова:
//     1. хар-ка sсr-файла
//          1:5 - кол-во экстентов
//          37:42 - мат.номер
//     2. хар-ка следующего файла
//        ........................
//        нулевая ячейка
//
        break;
    case E57_File_Info::FILE_RELEASE:
        throw Exception("Extracode *57 77777: operation 'Release File' not supported yet");
        break;
    case E57_File_Info::ALL_RELEASE:
        throw Exception("Extracode *57 77777: operation 'Release All' not supported yet");
        break;
    case E57_File_Info::FILE_CONTROL:
        // File name is located at addr.
        throw Exception("Extracode *57 77777: operation 'Change File Status' not supported yet");
        break;
    default:
        throw Exception("Extracode *57 77777: unknown operation " + to_octal(info.field.op));
    }
}
