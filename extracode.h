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

#pragma pack(push, 1)

//
// Экстракод 070: обмен с внешней памятью.
//
// Информационное слово по исполнительному адресу задаёт параметры обмена.
// Если исполнительный адрес равен нулю - информационное слово находится на сумматоре.
//
union E70_Info {
    Word word;

    struct {
        // При обмене с магнитными дисками:
        // Разряд 40 = 0 - запись листа ОП в зону МД;
        //           = 1 - чтение зоны МД в лист ОП;
        // Разряды 35-31 - номер листа;
        // Разряды 18-13 - логический номер МД;
        // Разряды 12-1  - номер зоны.
        //
        unsigned zone : 12;   // Zone number
        unsigned unit : 6;    // Disk unit number
        unsigned _1 : 12;     // ---
        unsigned page : 5;    // Memory page number
        unsigned _2 : 4;      // ---
        unsigned read_op : 1; // Operation: 1=Read, 0=Write
        unsigned seek : 1;    // Speculative operation: no data transfer
        unsigned _3 : 7;      // ---
    } disk;

    struct {
        // При обмене с магнитными барабанами:
        // Разряд 48 = 0 - обмен между листом ОП и трактом МБ;
        //           = 1 - секторный обмен между абзацем листа и сектором МБ;
        // Разряд 40 = 0 - запись из ОП на МБ;
        //           = 1 - чтение с МБ в ОП;
        // Разряд 39     - признак физ.обмена, перенаправляемого на первый заказанный диск,
        //                 с учётом смещения относительно номера ФИЗ МБ в паспорте;
        // Разряд 36     - для секторного обмена,
        //                 номер тракта в битах 7-3, номер сектора в битах 2-1;
        // Разряды 35-31 - номер листа;
        // Разряды 26-25 - номер абзаца в листе;
        // Разряды 18-13 - логический номер МБ;
        // Разряды 8-7   - номер сектора в тракте;
        // Разряды 5-1   - номер тракта.
        //
        unsigned tract : 5;     // Tract number
        unsigned _1 : 1;        // ---
        unsigned sector : 2;    // Sector number in the tract
        unsigned _2 : 4;        // ---
        unsigned unit : 6;      // Disk unit number
        unsigned _3 : 6;        // ---
        unsigned paragraph : 2; // Paragraph number in the memory page
        unsigned _4 : 4;        // ---
        unsigned page : 5;      // Memory page number
        unsigned raw_sect : 1;  // Raw sector number instead of tract/sector
        unsigned _5 : 2;        // ---
        unsigned phys_io : 1;   // Redirection to physical disk
        unsigned read_op : 1;   // Operation: 1=Read, 0=Write
        unsigned _6 : 7;        // ---
        unsigned sect_io : 1;   // When 1 - transfer only one sector
    } drum;
};

//
// Экстракод 064: выдача в выходной поток печати.
//
// В качестве информации к экстракоду задаются указатель
// выдаваемого массива и последовательность информационных слов "разметки".
//
// Указатель выдаваемого массива задает начальный и конечный
// адреса выдаваемого массива. Каждый из этих адресов
// может представляться парой "индекс-регистр - смещение".
//
union E64_Pointer {
    Word word;

    struct {
        // Разряды 48-45 - индекс-регистр, содержимое которого участвует
        //                 в формировании начального адреса выдаваемого массива.
        // Разряды 39-25 - смещение, значение которого участвует в
        //                 формировании начального адреса выдаваемого массива.
        // Разряды 24-21 - индекс-регистр, содержимое которого участвует
        //                 в формировании конечного адреса выдаваемого массива.
        // Разряды 15-1  - смещение, значение которого участвует в
        //                 формировании конечного адреса выдаваемого массива.
        //
        unsigned end_addr : 15;   // End address: offset
        unsigned _1 : 5;          // ---
        unsigned end_reg : 4;     // End address: register index
        unsigned start_addr : 15; // Start address: offset
        unsigned flags : 5;       // Bits special for Dubna OS
        unsigned start_reg : 4;   // Start address: register index
    } field;
};

//
// Информационные слова разметки определяют формат печати
// и расположение выдаваемой информации на бумаге. Слова разметки
// используются последовательно, при необходимости - циклически
// (т.е. по исчерпании всех слов разметки происходит
// возврат к первому слову разметки). После завершения
// исполнения печати всего выдаваемого массива выполняется
// прогон бумаги: число пустых строк указывается в последнем
// слове разметки (которое не обязательно является последним
// использованным для печати).
//
// Каждое слово разметки обеспечивает печать определенного
// числа так называемых элементов печати. Элементом печати
// для нетекстовых форматов является слово оп, а для текстовых
// - последовательность слов, завершающаяся словом с символом
// "конец текста" (или концом выдаваемого массива).
//
// В каждом слове разметки задается:
//
//   - количество элементов `К`, печатаемых по данному слову разметки;
//   - номер позиции `А`, начиная с которой будет печататься
//     первый элемент, выдаваемый по данному слову разметки;
//   - отступ `D` от начальной позиции предыдущего элемента,
//     выполняемый перед печатью очередного элемента по данной разметке;
//   - количество `L` выдаваемых символов одного элемента.
//
union E64_Info {
    Word word;

    struct {
        // Разряды 48-45 - формат печати:
        //                 0,8 - текст в кодировке ГОСТ;
        //                 1,5,9,13 - команды;
        //                 2,10 - восьмеричное целое;
        //                 3,11 - машинное число с плавающей точкой;
        //                 4,12 - текст в кодировке автокода ИТМ;
        //                 6,7,14,15 - шестнадцатеричное целое;
        // Разряды 43-37 - номер позиции (с нуля), начиная с которой
        //                 должна выдаваться информация по данному
        //                 информационному слову (`А`);
        // Разряды 31-25 - количество печатаемых символов (`L`);
        // Разряд 24     - признак последнего информационного слова;
        // Разряды 23-21 - число протяжек без единицы (число пустых
        //                 строк), выдаваемых после завершения экстракода;
        //                 содержимое данного поля игнорируется при 24 разряде = 0
        //                 (т.е. во всех информационных словах разметки, кроме последнего);
        // Разряды 19-13 - интервал между начальными позициями элементов,
        //                 печатаемых по данному информационному слову разметки (`D`);
        // Разряды 7-1   - число элементов, печатаемых по данному
        //                 слову разметки, уменьшенное на единицу (`К-1`).
        //
        unsigned repeat1 : 7; // Repeat count (K) minus 1
        unsigned _1 : 5;      // ---
        unsigned width : 7;   // Interval between elements
        unsigned _2 : 1;      // ---
        unsigned skip : 3;    // Number of extra empty lines at the end
        unsigned finish : 1;  // Flag of the final info word
        unsigned digits : 7;  // Number of digits (L) to print
        unsigned _3 : 5;      // ---
        unsigned offset : 7;  // Position at which to start (A)
        unsigned _4 : 1;      // ---
        unsigned format : 4;  // Format of the output
    } field;
};

//
// Экстракод 057 77777: заказ файловых ресурсов.
//
// Информационное слово на сумматоре задаёт параметры обмена.
//
union E57_Request_Info {
    Word word;

    struct {
        // Разряды 15-1 - адрес информационного поля
        // Разряды 20-16 - код операции
        // Разряды 24-21, 48-30 содержат ключ доступа
        // Разряды 29-25 - дополнительная информация
        //
        unsigned addr : 15; // Address of info field
        unsigned op : 5;    // Operation
        unsigned key1 : 4;  // Access key, part 1
        unsigned flags : 5; // Additional info
        unsigned key2 : 19; // Access key, part 2
    } field;

    // Коды операций
    enum {
        VOLUME_OPEN    = 0,   // заказ пакетов
        VOLUME_RELEASE = 1,   // отказ от пакетов
        FILE_SEARCH    = 2,   // запрос координат файлов
        FILE_OPEN      = 3,   // заказ обычных файлов
        SCRATCH_OPEN   = 4,   // заказ scratch файлов
        FILE_RELEASE   = 5,   // отказ от файлов
        ALL_RELEASE    = 6,   // отказ от всех дисковых ресурсов
        FILE_CONTROL   = 037, // изменение статуса файла
    };

    // Access key
    static const Word KEY_VALUE   = 0'6470'7240'4000'0000;
    static const Word KEY_BITMASK = 0'7777'7740'7400'0000;
};

union E57_Scratch_Info {
    Word word;

    struct {
        // Разряды 5-1 - количество экстентов
        // Разряды 42-37 - мат.номер
        //
        unsigned size : 5;      // Size in blocks of 040 zones
        unsigned _1 : 31;       // ---
        unsigned disk_unit : 6; // Disk unit number
        unsigned _2 : 6;        // ---
    } field;
};

union E57_Search_Info {
    Word word[4];

    struct {
        // Name of owner as ISO string, up to 6 characters.
        Word owner;

        // File name as ISO string, up to 6 characters.
        Word file_name;

        // Bit 30 means write mode. Put reply here.
        unsigned offset : 29;       // Result: file offset on disk
        unsigned write_mode : 1;    // 0-Read only, 1-Write/read
        unsigned long long _1 : 12; // ---
        unsigned error : 6;         // Return status here

        // Разряды 6-1 - мат.номер
        // Разряд 48 всегда единица
        //
        unsigned disk_unit : 6;     // Disk unit to mount
        unsigned long long _2 : 41; // ---
        unsigned bit48 : 1;         // Always 1
    } field;
};

union E57_Open_Info {
    Word word;

    struct {
        // Bit 30 means write mode. Put reply here.
        unsigned offset : 29;      // Result: file offset on disk
        unsigned write_mode : 1;   // 0-Read only, 1-Write/read
        unsigned long long _1 : 6; // ---
        unsigned disk_unit : 6;    // Disk unit to mount
        unsigned error : 6;        // Return status here
    } field;
};

//
// В поле ответа для каждого файла будут признаки всяких бяк.
//
enum {
    E57_DISK_OFFLINE = 010, // бит 43 - нет диска
    E57_DISK_BUSY    = 020, // бит 44 - занят мат.номер
    E57_FILE_BUSY    = 040, // бит 45 - занят файл
    E57_NO_ACCESS    = 010, // бит 46 - нет доступа к файлу или нет места для файла
    E57_NOT_FOUND    = 020, // бит 47 - файла нет на диске
    E57_BAD_REQUEST  = 040, // бит 48 - плохое обращение
};

//
// Экстракод 050 015, 016, 017: форматные преобразования из двоичного кода в ISO.
//
// Информационное слово на сумматоре задаёт параметры обмена.
//
// Результат работы для всех трех экстракодов: сумматор - длина
// выходной строки в символах, и.р.14 = 1, если превышен размер поля,
// иначе и.р.14 = 0. В каждый момент времени последнее
// слово выходной строки дополнено до конца пробелами.
//
union E50_Format_Info {
    Word word;

    struct {
        // Разряды 15-1 - адрес выходной строки или 0, если результат
        //                пишется с текущей позиции выходной строки
        // Разряды 19-16 - число цифр мантиссы
        // Разряд 20 - флаг выравнивания текста внутри своего поля (1 - вправо, 0 - влево)
        // Разряды 24-21 - индекс.регистр выходной строки
        // Разряды 39-25 - адрес входного числа
        // Разряды 44-40 - число позиций для размещения полученного текстового фрагмента
        // Разряды 48-45 - индекс.регистр входного числа
        //
        unsigned dest_addr : 15;  // Address of output text
        unsigned precision : 4;   // Number of digits in mantissa
        unsigned right_align : 1; // 1 - right align, 0 - left align
        unsigned dest_reg : 4;    // Register of output address
        unsigned src_addr : 15;   // Address of input value
        unsigned width : 5;       // Width of resulting text
        unsigned src_reg : 4;     // Register of input address
    } field;
};

//
// e50 067 - Get date and time
//
union E50_Date_Time {
    Word word;

    struct {
        unsigned decisec : 4;  // bits 4-1
        unsigned sec_lo : 4;   // bits 8-5
        unsigned sec_hi : 4;   // bits 12-9
        unsigned min_lo : 4;   // bits 16-13
        unsigned min_hi : 4;   // bits 20-17
        unsigned hour_lo : 4;  // bits 24-21
        unsigned hour_hi : 2;  // bits 26-25
        unsigned year_lo : 4;  // bits 30-27
        unsigned year_hi : 4;  // bits 34-31
        unsigned month_lo : 4; // bits 38-35
        unsigned month_hi : 4; // bits 42-39
        unsigned day_lo : 4;   // bits 46-43
        unsigned day_hi : 2;   // bits 48-47
    } field;
};
#pragma pack(pop)

#endif // DUBNA_EXTRACODE_H
