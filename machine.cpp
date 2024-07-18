//
// BESM-6: Big Electronic Calculating Machine, model 6.
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

#include <unistd.h>

#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>

#include "encoding.h"

// Static fields.
bool Machine::verbose                    = false;
bool Machine::keep_temporary_files       = false;
uint64_t Machine::simulated_instructions = 0;

// Limit of instructions, by default.
const uint64_t Machine::DEFAULT_LIMIT = 100ULL * 1000 * 1000 * 1000;

//
// Initialize the machine.
//
Machine::Machine(Memory &m)
    : progress_time_last(std::chrono::steady_clock::now()), memory(m),
      cpu(*this, m), puncher(m)
{
}

//
// Deallocate the machine: disable tracing.
//
Machine::~Machine()
{
    redirect_trace(nullptr, "");
    enable_trace("");
}

//
// Every few seconds, print a message to stderr, to track the simulation progress.
//
void Machine::show_progress()
{
    //
    // Check the real time every few thousand cycles.
    //
    static const uint64_t PROGRESS_INCREMENT = 10000;

    if (simulated_instructions >= progress_count + PROGRESS_INCREMENT) {
        progress_count += PROGRESS_INCREMENT;

        // How much time has passed since the last check?
        auto time_now = std::chrono::steady_clock::now();
        auto delta    = time_now - progress_time_last;
        auto sec      = std::chrono::duration_cast<std::chrono::seconds>(delta).count();

        // Emit message every 5 seconds.
        if (sec >= 5) {
            std::cerr << "----- Progress " << simulated_instructions << " -----" << std::endl;
            progress_time_last = time_now;
        }
    }
}

//
// Run the machine until completion.
//
void Machine::run()
{
again:
    // Show initial state.
    trace_registers();

    try {
        for (;;) {
            bool done = cpu.step();

            // Show changed registers.
            trace_registers();

            if (progress_message_enabled) {
                show_progress();
            }
            simulated_instructions++;
            if (simulated_instructions > instr_limit)
                throw std::runtime_error("Simulation limit exceeded");

            if (done) {
                // Halted by 'стоп' instruction.
                cpu.finish();
                return;
            }
        }

    } catch (const Processor::Exception &ex) {
        // Unexpected situation in the machine.
        cpu.stack_correction();
        cpu.finish();

        auto const *message = ex.what();
        if (!message[0]) {
            // Empty message - legally halted by extracode e74.
            return;
        }
        std::cerr << "Error: " << message << std::endl;
        //trace_exception(message);

        if (cpu.intercept(message)) {
            goto again;
        }
        throw std::runtime_error(message);

    } catch (std::exception &ex) {
        // Something else.
        cpu.finish();
        std::cerr << "Error: " << ex.what() << std::endl;
        throw std::runtime_error(ex.what());
    }
}

//
// Fetch instruction word.
//
Word Machine::mem_fetch(unsigned addr)
{
    if (addr == 0) {
        throw Processor::Exception("Jump to zero");
    }

    Word val = memory.load(addr);

    if (!cpu.on_right_instruction()) {
        trace_fetch(addr, val);
    }
    return val & BITS48;
}

//
// Write word to memory.
//
void Machine::mem_store(unsigned addr, Word val)
{
    addr &= BITS(15);
    if (addr == 0)
        return;

    memory.store(addr, val);
    trace_memory_write(addr, val);
}

//
// Read word from memory.
//
Word Machine::mem_load(unsigned addr)
{
    addr &= BITS(15);
    if (addr == 0)
        return 0;

    Word val = memory.load(addr);
    trace_memory_read(addr, val);

    return val & BITS48;
}

//
// Load input job from file.
// Throw exception on failure.
//
void Machine::load(const std::string &filename)
{
    // Open the input file.
    std::ifstream input;
    input.open(filename);
    if (!input.is_open())
        throw std::runtime_error(filename + ": " + std::strerror(errno));

    load(input);
}

//
// Load input job from stream to drum #1.
//
void Machine::load(std::istream &input)
{
    // Word offset from the beginning of the drum.
    unsigned offset = 0;

    while (input.good()) {
        if (offset >= 040 * PAGE_NWORDS)
            throw std::runtime_error("Input job is too large");

        // Get next line.
        std::string line;
        getline(input, line);
        if (line[0] == '`') {
            Word word = std::stoul(line.c_str() + 1, nullptr, 8);
            drum_write_word(1, offset++, word);
        } else
        // Write to drum in COSY format.
        drum_write_cosy(1, offset, line);
    }
}

//
// Encode line as COSY and write to drum.
// Update offset.
//
void Machine::drum_write_cosy(unsigned drum_unit, unsigned &offset, const std::string &input)
{
    // Convert input from UTF-8 to KOI-7.
    // Encode as COSY.
    std::string line = encode_cosy(utf8_to_koi7(input));

    // Write to drum as words.
    for (; line.size() >= 6; line.erase(0, 6)) {
        Word word = (uint8_t)line[0];
        word      = word << 8 | (uint8_t)line[1];
        word      = word << 8 | (uint8_t)line[2];
        word      = word << 8 | (uint8_t)line[3];
        word      = word << 8 | (uint8_t)line[4];
        word      = word << 8 | (uint8_t)line[5];
        drum_write_word(drum_unit, offset++, word);
    }
}

//
// Disk read/write.
//
void Machine::disk_io(char op, unsigned disk_unit, unsigned zone, unsigned sector, unsigned addr,
                      unsigned nwords)
{
    Disk *disk;

    if (disk_unit == PHYS_IO_UNIT) {
        if (!mapped_disk) {
            // Disk must be previously mapped using map_drum_to_disk().
            throw std::runtime_error("No disk unit mapped for phys.io");
        }
        disk = mapped_disk.get();
    } else {
        if (disk_unit >= NDRUMS) {
            throw std::runtime_error("Invalid disk unit " + to_octal(disk_unit));
        }
        if (!disks[disk_unit]) {
            // Disk must be previously configured using disk_mount().
            throw std::runtime_error("Disk unit " + to_octal(disk_unit + 030) + " is not mounted");
        }
        disk = disks[disk_unit].get();
    }

    if (op == 'r') {
        disk->disk_to_memory(zone, sector, addr, nwords);

        // Debug: dump the data.
        if (dump_io_flag) {
            auto unit = (disk_unit == PHYS_IO_UNIT) ? 0 : (disk_unit + 030);
            memory.dump(++dump_serial_num, unit, zone, sector, addr, nwords);
        }
    } else {
        disk->memory_to_disk(zone, sector, addr, nwords);
    }
}

//
// Drum read/write.
//
void Machine::drum_io(char op, unsigned drum_unit, unsigned zone, unsigned sector, unsigned addr,
                      unsigned nwords)
{
    drum_init(drum_unit);
    if (op == 'r') {
        drums[drum_unit]->drum_to_memory(zone, sector, addr, nwords);
    } else {
        drums[drum_unit]->memory_to_drum(zone, sector, addr, nwords);
    }
}

//
// Write one word to drum.
//
void Machine::drum_write_word(unsigned drum_unit, unsigned offset, Word value)
{
    drum_init(drum_unit);
    drums[drum_unit]->write_word(offset, value);
}

//
// Read one word from drum.
//
Word Machine::drum_read_word(unsigned drum_unit, unsigned offset)
{
    drum_init(drum_unit);
    return drums[drum_unit]->read_word(offset);
}

//
// Allocate drum on first access.
//
void Machine::drum_init(unsigned drum_unit)
{
    if (drum_unit >= NDRUMS)
        throw std::runtime_error("Invalid drum unit " + to_octal(drum_unit));

    if (!drums[drum_unit]) {
        // Allocate new drum on first request.
        drums[drum_unit] = std::make_unique<Drum>(memory);
    }
}

//
// Open binary image and assign it to the disk unit.
//
void Machine::disk_mount(unsigned disk_unit, Word tape_id, bool write_permit)
{
    if (disk_unit < 030 || disk_unit >= 070)
        throw std::runtime_error("Invalid disk unit " + to_octal(disk_unit) + " in disk_mount()");

    disk_unit -= 030;
    if (disks[disk_unit]) {
        // Already mounted.
        auto mounted_id = disks[disk_unit]->get_id();
        if (mounted_id == tape_id) {
            // The same ID - disk is already opened.
            return;
        }
        throw std::runtime_error("Disk unit " + to_octal(disk_unit + 030) +
                                 " is already mounted as " + tape_name_string(mounted_id));
    }

    // Open binary image as disk.
    auto path        = disk_path(tape_id);
    disks[disk_unit] = std::make_unique<Disk>(tape_id, memory, path, write_permit);

    if (trace_enabled()) {
        std::cout << "Mount image '" << path << "' as disk " << to_octal(disk_unit + 030) << std::endl;
    }
}

//
// Create scratch file and assign it to the disk unit.
//
void Machine::scratch_mount(unsigned disk_unit, unsigned num_zones)
{

    if (disk_unit < 030 || disk_unit >= 070)
        throw std::runtime_error("Invalid disk unit " + to_octal(disk_unit) + " in scratch_mount()");

    const auto digit_lo = disk_unit % 8;
    const auto digit_hi = disk_unit / 8;
    const Word tape_id = TAPE_SCRATCH | (digit_hi << 4) | digit_lo;

    disk_unit -= 030;
    if (disks[disk_unit]) {
        // Already mounted.
        auto mounted_id = disks[disk_unit]->get_id();
        throw std::runtime_error("Disk unit " + to_octal(disk_unit + 030) +
                                 " is already mounted as " + tape_name_string(mounted_id));
    }

    // Create temporary file.
    std::string pattern = "scratch" + std::to_string(digit_hi) + std::to_string(digit_lo);
    disks[disk_unit]    = std::make_unique<Disk>(tape_id, memory, pattern, num_zones);
    auto &path          = disks[disk_unit]->get_path();

    if (trace_enabled()) {
        std::cout << "Mount image '" << path
                  << "' as disk " << to_octal(disk_unit + 030) << std::endl;
    }
    if (!keep_temporary_files) {
        // Remove temporary file.
        std::remove(path.c_str());
    }
}

//
// Release volumes according to bitmask on accumulator.
//
void Machine::disk_release(Word bitmask)
{
    if (trace_enabled()) {
        std::cout << "\nRelease tapes 0" << std::oct << bitmask << std::dec << '\n';
    }
    for (unsigned disk_unit = 0; disk_unit < NDISKS; disk_unit++) {
        bool release_flag = (bitmask >> (47 - disk_unit)) & 1;
        if (release_flag && disks[disk_unit]) {
            if (trace_enabled()) {
                auto mounted_id = disks[disk_unit]->get_id();
                std::cout << "Release disk " << std::oct << (disk_unit + 030) << std::dec
                          << ", mounted as " << tape_name_string(mounted_id) << '\n';
            }
            disks[disk_unit].reset();
        }
    }
}

//
// Find opened disk by tape ID.
//
unsigned Machine::disk_find(Word tape_id)
{
    if (tape_id == TAPE_MONSYS) {
        // Tape MONSYS in mounted on direction #30.
        return 030;
    } else {
        std::cout << "\nTape not found: " << tape_name_string(tape_id) << '\n';
        return 0;
    }
}

//
// Redirect drum to disk.
// It's called Phys.IO in Dispak.
//
void Machine::map_drum_to_disk(unsigned drum, unsigned disk_unit)
{
    // Clone the disk.
    mapped_disk = std::make_unique<Disk>(*disks[disk_unit - 030].get());
    mapped_drum = drum;
    if (trace_enabled()) {
        std::cout << "Redirect drum " << to_octal(mapped_drum) << " to disk " << to_octal(disk_unit)
                  << std::endl;
    }
}

//
// Find file at pre-defined places.
// 1. Use envorinment variable BESM6_PATH.
// 2. Try ~/.besm6 directory.
// 3. Try /usr/local/share/besm6 directory.
//
std::string Machine::disk_path(Word tape_id)
{
    // Build file name from tape number.
    const unsigned tape_num = ((tape_id >> 8) & 0xf) * 100 +
                              ((tape_id >> 4) & 0xf) * 10 +
                               (tape_id & 0xf);
    const std::string filename = std::to_string(tape_num);
    //std::cout << "\n--- tape id 0" << std::oct << tape_id << std::dec
    //          << ' ' << tape_name_string(tape_id)
    //          << ", filename " << filename << '\n';

    // Setup the list of directories to search.
    if (disk_search_path.empty()) {
        auto env_besm6_path = std::getenv("BESM6_PATH");
        if (env_besm6_path != nullptr) {
            disk_search_path = env_besm6_path;
        } else {
            // No BESM6_PATH, so use the default.
            auto env_home = std::getenv("HOME");
            if (env_home != nullptr) {
                disk_search_path = env_home;
            }
            disk_search_path += "/.besm6:/usr/local/share/besm6";
        }
    }

    // Iterate the search path using string stream.
    std::stringstream list(disk_search_path);
    while (list.good()) {
        // Get directory name from the list.
        std::string name;
        getline(list, name, ':');

        // Check for disk image here.
        name += "/";
        name += filename;
        if (access(name.c_str(), R_OK) >= 0) {
            return name;
        }
    }
    throw std::runtime_error("Tape " + tape_name_string(tape_id) + " not found");
}

//
// Return tape name as string.
// Name consists of up to 6 characters in TEXT encoding,
// and up to three decimal digits in 2-10 format.
//
std::string tape_name_string(Word tape_id)
{
    std::ostringstream buf;
    unsigned const num = ((tape_id >> 8) & 0xf) * 100 +
                         ((tape_id >> 4) & 0xf) * 10 +
                         (tape_id & 0xf);
    buf << num << '/'
        << (char) std::tolower(text_to_unicode(tape_id >> 42))
        << (char) std::tolower(text_to_unicode(tape_id >> 36))
        << (char) std::tolower(text_to_unicode(tape_id >> 30))
        << (char) std::tolower(text_to_unicode(tape_id >> 24))
        << (char) std::tolower(text_to_unicode(tape_id >> 18))
        << (char) std::tolower(text_to_unicode(tape_id >> 12));
    return buf.str();
}

//
// Decode word as string in ISO format.
//
std::string word_iso_string(Word w)
{
    std::ostringstream buf;
    for (int shift = 40; shift >= 0; shift -= 8) {
        uint8_t ch = w >> shift;
        iso_putc(ch, buf);
    }
    return buf.str();
}

//
// Send message to operator's console.
//
void Machine::print_iso_string(std::ostream &out, unsigned addr)
{
    BytePointer bp(memory, ADDR(addr));
    for (;;) {
        auto ch = bp.get_byte();
        if (ch == '\0')
            break;
        iso_putc(ch, out);
    }
    out << '\n';
}

//
// Load boot code for Monitoring System Dubna.
//
void Machine::boot_ms_dubna(const std::string &path)
{
    // Mount tape 9/monsys as disk 30, read only.
    disk_search_path = path;
    disk_mount_readonly(030, TAPE_MONSYS);

    // Phys i/o: re-direct drum 021 to disk 030.
    map_drum_to_disk(021, 030);

    //
    // I got this magic code from Mikhail Popov.
    // See https://groups.google.com/g/besm6/c/e5jM_R1Oozc/m/aGfCePzsCwAJ
    // It more or less coincides with sources of STARTJOB routine
    // at https://github.com/besm6/besm6.github.io/blob/master/sources/dubna/cross/extold.txt#L250
    //
    // clang-format off
    memory.store(02010, besm6_asm("vtm -5(1),     *70 3002"));     // читаем инициатор монитора
    memory.store(02011, besm6_asm("xta 377,       atx 3010"));     // берем тракт, где MONITOR* + /MONTRAN
    memory.store(02012, besm6_asm("xta 363,       atx 100"));      // ТРП для загрузчика
    memory.store(02013, besm6_asm("vtm 53401(17), utc"));          // магазин
    memory.store(02014, besm6_asm("*70 3010(1),   utc"));          // каталоги
    memory.store(02015, besm6_asm("vlm 2014(1),   ita 17"));       // aload по адресу 716b
    memory.store(02016, besm6_asm("atx 716,       *70 717"));      // infloa по адресу 717b - статический загрузчик
    memory.store(02017, besm6_asm("xta 17,        ati 16"));       //
    memory.store(02020, besm6_asm("atx 2(16),     arx 3001"));     // прибавляем 10b
    memory.store(02021, besm6_asm("atx 17,        xta 3000"));     // 'INPUTCAL'
    memory.store(02022, besm6_asm("atx (16),      vtm 1673(15)")); // call CHEKJOB*
    memory.store(02023, besm6_asm("uj (17),       utc"));          // в статический загрузчик

    memory.store(03000, 0'5156'6065'6443'4154ul); // 'INPUTCAL' in Text encoding
    memory.store(03001, 0'0000'0000'0000'0010ul); // прибавляем 10b
    memory.store(03002, 0'4014'0000'0021'0201ul); // инициатор
    memory.store(03003, 0'0000'0000'0020'0000ul); // таблица резидентных программ
    memory.store(03004, 0'0014'0000'0021'0007ul); // каталоги
    memory.store(03005, 0'0000'0000'0021'0000ul); // временной
    memory.store(03006, 0'0014'0000'0021'0010ul); // библиотеки
    memory.store(03007, 0'0000'0000'0021'0001ul); // (физ. и мат.)
    memory.store(03010, 0'0014'0000'0021'0035ul); // /MONTRAN
    // clang-format on

    cpu.set_pc(02010);
}
