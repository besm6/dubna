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
        // Tape control by Gusev.
        throw Exception("*57 " + to_octal(addr) + ": Tape drive is not supported");
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
    E57_Request_Info info;
    info.word = core.ACC;

    machine.trace_e57_request(info);

    if ((info.word & E57_Request_Info::KEY_BITMASK) != E57_Request_Info::KEY_VALUE) {
        throw Exception("Wrong access key in *57 77777");
    }
    switch (info.field.op) {
    case E57_Request_Info::VOLUME_OPEN: {
        //
        // Структура задания:
        //      0. признак монополии на диск
        //      1. имя диска
        //
        Word disc_id = machine.mem_load(info.field.addr + 1);
        switch (disc_id & ~0xfff) {
        case Machine::DISC_LOCAL:
        case Machine::DISC_HOME:
        case Machine::DISC_TMP:
            // Valid disc name.
            core.ACC = 0;
            break;
        default:
            throw Exception("Unsupported disc name: " + tape_name_string(disc_id));
        }
        break;
    }
    case E57_Request_Info::VOLUME_RELEASE:
        throw Exception("Extracode *57 77777: operation 'Release Volume' not supported yet");
        break;

    case E57_Request_Info::FILE_SEARCH: {
        //
        // Структура задания:
        //      0. имя диска
        //    1-4. запрос координат одного файла
        //    5-8. запрос координат следующего файла
        //         .........................
        //         запрос координат последнего файла
        //  N*4+1. бит 48
        //
        Word disc_id = machine.mem_load(info.field.addr);
        for (auto addr = info.field.addr + 1; ; addr += 4) {
            E57_Search_Info item;
            item.word[0] = machine.mem_load(addr);
            if (item.word[0] == BIT48) {
                break;
            }

            // Запрос координат файла выглядит так:
            //      0. имя хозяина файла
            //      1. имя файла
            //      2. поле ответа (+бит 30, если запись)
            //      3. бит 48 + мат.номер файла в 1:6 р.
            //
            item.word[1] = machine.mem_load(addr + 1);
            item.word[2] = machine.mem_load(addr + 2);
            item.word[3] = machine.mem_load(addr + 3);
            machine.trace_e57_search(item);

            item.field.offset = machine.file_search(disc_id, item.field.file_name, item.field.write_mode);
            if (!item.field.offset) {
                // Failed with this file.
                if (item.field.write_mode) {
                    item.field.error = E57_NO_ACCESS;
                } else {
                    item.field.error = E57_NOT_FOUND;
                }
            }
            machine.mem_store(addr + 2, item.word[2]);
        }
        core.ACC = 0;
        break;
    }

    case E57_Request_Info::FILE_OPEN:
        //
        // Структура задания:
        //      0. имя диска
        //      1. координаты первого файла (берутся из поля ответа предыдущего стека + мат.номер в 37:42 р.)
        //      2. координаты второго файла
        //         ...........................
        //         координаты последнего файла
        //    N+1. нулевая ячейка
        //
        //Word disc_id = machine.mem_load(info.field.addr);
        for (auto addr = info.field.addr + 1; ; addr++) {
            E57_Open_Info item;
            item.word = machine.mem_load(addr);
            if (!item.word) {
                break;
            }
            machine.trace_e57_open(item);

            item.field.error = machine.file_mount(item.field.disk_unit, item.field.offset, item.field.write_mode);
            machine.mem_store(addr, item.word);
        }
        core.ACC = 0;
        break;

    case E57_Request_Info::SCRATCH_OPEN:
        // Allocate scratch file.
        //
        // Структура инф.поля такова:
        //      0. характеристика sсratch-файла
        //          1:5 - количество экстентов
        //          37:42 - мат.номер
        //      1. характеристика следующего файла
        //         ........................
        //      N. нулевая ячейка
        //
        for (auto addr = info.field.addr; ; addr++) {
            E57_Scratch_Info item;
            item.word = machine.mem_load(addr);
            if (item.word == 0) {
                break;
            }
            machine.trace_e57_scratch(item);
            machine.scratch_mount(item.field.disk_unit, item.field.size * 040);
        }
        core.ACC = 0;
        break;

    case E57_Request_Info::FILE_RELEASE:
        throw Exception("Extracode *57 77777: operation 'Release File' not supported yet");
        break;

    case E57_Request_Info::ALL_RELEASE:
        throw Exception("Extracode *57 77777: operation 'Release All' not supported yet");
        break;

    case E57_Request_Info::FILE_CONTROL:
        // File name is located at addr.
        throw Exception("Extracode *57 77777: operation 'Change File Status' not supported yet");
        break;

    default:
        throw Exception("Extracode *57 77777: unknown operation " + to_octal(info.field.op));
    }
}
