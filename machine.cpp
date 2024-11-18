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

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>

#include "encoding.h"
#include "cosy.h"

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
    : progress_time_last(std::chrono::steady_clock::now()), memory(m), cpu(*this, m), puncher(m)
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

            if (progress_message_enabled && !entropy_flag) {
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
        auto pc = cpu.get_pc();
        cpu.stack_correction();
        cpu.finish();

        auto const *message = ex.what();
        if (!message[0]) {
            // Empty message - legally halted by extracode e74.
            return;
        }
        std::cerr << "Error: " << message << " @"
                  << std::oct << std::setfill('0') << std::setw(5) << pc << std::endl;
        // trace_exception(message);

        if (cpu.intercept(message)) {
            goto again;
        }
        throw std::runtime_error(message);

    } catch (std::exception &ex) {
        // Something else.
        cpu.finish();
        //std::cerr << "Error: " << ex.what() << std::endl;
        throw std::runtime_error(ex.what());
    }
}

//
// Save output files.
//
void Machine::finish()
{
    // Save plotter data.
    plotter.finish(keep_temporary_files);

    // Save puncher data.
    puncher.finish();

    // Remove temporary files.
    for (auto &disk : disks) {
        if (disk) {
            disk->finish();
        }
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
void Machine::load_job(const std::string &filename)
{
    if (trace_enabled()) {
        std::cout << "Read job '" << filename << "'\n";
    }

    // Open the input file.
    std::ifstream input;
    input.open(filename);
    if (!input.is_open())
        throw std::runtime_error(filename + ": " + std::strerror(errno));

    load_job(input);
}

//
// Load input job from stream to drum #1.
//
void Machine::load_job(std::istream &input)
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
        } else {
            // Write to drum in COSY format.
            drum_write_cosy(1, offset, line);
        }
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
        std::cout << "Mount image '" << path << "' as disk " << to_octal(disk_unit + 030)
                  << std::endl;
    }
}

//
// Find file by name on given disk.
// Return unique 'offset' of the file on the disk.
// In case of error return zero.
//
unsigned Machine::file_search(Word disc_id, Word file_name, bool write_mode)
{
    // Get directory path, based on disc name.
    std::string dir;
    switch (disc_id & ~0xfff) {
    case DISC_LOCAL:
        dir = ".";
        break;
    case DISC_HOME:
        dir = std::getenv("HOME");
        if (dir.size() == 0)
            return 0;
        break;
    case DISC_TMP:
        dir = "/tmp";
        break;
    default:
        return 0;
    }
    const std::string path = dir + "/" + word_iso_filename(file_name) + ".bin";

    // Let's figure out whether file exists.
    bool file_exists{};
    {
        // Try to read the file.
        std::ifstream file(path);
        file_exists = file.good();
    }

    if (write_mode) {
        // Write mode: try to write file.
        std::fstream file(path, std::ios::out);
        bool is_writable = file.good();
        if (!file_exists) {
            // Remove the file we just created.
            file.close();
            std::filesystem::remove(path);
        }
        if (!is_writable) {
            return 0;
        }
    } else {
        // Read mode: the file must exist.
        if (!file_exists) {
            // Check for *.txt instead.
            const std::string txt_path = dir + "/" + word_iso_filename(file_name) + ".txt";
            std::ifstream file(txt_path);
            file_exists = file.good();
            if (!file_exists) {
                return 0;
            }
        }
    }

    // Append file path to the list.
    // This list always increases, never shrinks.
    // Return file index (plus 1).
    file_paths.push_back(path);
    if (trace_enabled()) {
        std::cout << "Access file '" << path << "' at index " << file_paths.size() << std::endl;
    }
    return file_paths.size();
}

//
// Open file and assign it to the disk unit.
// Return error code.
// In case of success return zero.
//
unsigned Machine::file_mount(unsigned disk_unit, unsigned file_index, bool write_mode)
{
    if (disk_unit < 030 || disk_unit >= 070)
        throw std::runtime_error("Invalid disk unit " + to_octal(disk_unit) + " in file_mount()");

    if (disks[disk_unit - 030]) {
        // Already mounted.
        return E57_DISK_BUSY;
    }

    // Get file path.
    // Note file index has offset +1.
    auto const &path = file_paths[file_index - 1];

    // When file.bin is absent, but file.txt exists -
    //  * convert file.txt -> file.bin
    //  * in read-only mode: set flag to remove file.bin when finished
    //
    bool bin_created = file_txt_to_cosy(path);

    if (write_mode) {
        // Create file and close it.
        std::fstream file(path, std::ios::out);
        if (!file.good()) {
            return E57_NO_ACCESS;
        }
    }
    disks[disk_unit - 030] = std::make_unique<Disk>(0, memory, path, write_mode);
    if (bin_created && !write_mode) {
        // Remove binary image of the disk when finished.
        disks[disk_unit - 030]->remove_when_finished();
    }

    if (trace_enabled()) {
        std::cout << "Mount file '" << path << "' as disk " << to_octal(disk_unit) << std::endl;
    }
    return 0;
}

//
// Create scratch file and assign it to the disk unit.
//
void Machine::scratch_mount(unsigned disk_unit, unsigned num_zones)
{
    if (disk_unit < 030 || disk_unit >= 070)
        throw std::runtime_error("Invalid disk unit " + to_octal(disk_unit) +
                                 " in scratch_mount()");

    const auto digit_lo = disk_unit % 8;
    const auto digit_hi = disk_unit / 8;
    const Word tape_id  = TAPE_SCRATCH | (digit_hi << 4) | digit_lo;

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
        std::cout << "Mount image '" << path << "' as disk " << to_octal(disk_unit + 030)
                  << std::endl;
    }
    if (!keep_temporary_files) {
        // Remove temporary file.
        std::filesystem::remove(path);
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
    const unsigned tape_num =
        ((tape_id >> 8) & 0xf) * 100 + ((tape_id >> 4) & 0xf) * 10 + (tape_id & 0xf);
    const std::string filename = std::to_string(tape_num);
    // std::cout << "\n--- tape id 0" << std::oct << tape_id << std::dec
    //           << ' ' << tape_name_string(tape_id)
    //           << ", filename " << filename << '\n';

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
    unsigned const num =
        ((tape_id >> 8) & 0xf) * 100 + ((tape_id >> 4) & 0xf) * 10 + (tape_id & 0xf);
    buf << num << '/' << (char)std::tolower(text_to_unicode(tape_id >> 42))
        << (char)std::tolower(text_to_unicode(tape_id >> 36))
        << (char)std::tolower(text_to_unicode(tape_id >> 30))
        << (char)std::tolower(text_to_unicode(tape_id >> 24))
        << (char)std::tolower(text_to_unicode(tape_id >> 18))
        << (char)std::tolower(text_to_unicode(tape_id >> 12));
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
// Decode word as 8 characters in TEXT format.
//
std::string word_text_string(Word w)
{
    std::ostringstream buf;
    for (int shift = 42; shift >= 0; shift -= 6) {
        unsigned ch = text_to_unicode(w >> shift);
        utf8_putc(ch, buf);
    }
    return buf.str();
}

//
// Decode word as filename in ISO format.
// Remove trailing spaces, convert to lowercase.
//
std::string word_iso_filename(Word w)
{
    std::string filename = word_iso_string(w);

    // Remove trailing spaces.
    filename.erase(filename.find_last_not_of(" ") + 1);

    // Convert to lowercase.
    std::transform(filename.begin(), filename.end(), filename.begin(),
                   [](unsigned char c) { return std::tolower(c); });
    return filename;
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
    memory.store(02010, besm6_asm("vtm -5(1),     *70 3002"));     // читаем таблицу резидентных программ для загрузчика
    memory.store(02011, besm6_asm("xta 377,       atx 3010"));     // берем тракт, где MONITOR* + /MONTRAN
    memory.store(02012, besm6_asm("xta 363,       atx 100"));      // восстановим испорченный IОLISТ*
    memory.store(02013, besm6_asm("vtm 53401(17), utc"));          // магазин
    memory.store(02014, besm6_asm("*70 3010(1),   utc"));          // каталоги
    memory.store(02015, besm6_asm("vlm 2014(1),   ita 17"));       // aload по адресу 716b
    memory.store(02016, besm6_asm("atx 716,       *70 717"));      // infloa по адресу 717b - статический загрузчик
    memory.store(02017, besm6_asm("xta 17,        ati 16"));       //
    memory.store(02020, besm6_asm("atx 2(16),     arx 3001"));     // прибавляем 10b
    memory.store(02021, besm6_asm("atx 17,        xta 3000"));     // 'INPUTCAL'
    memory.store(02022, besm6_asm("atx (16),      vtm 1673(15)")); // call CHEKJOB*
    if (system_load_list_flag) {
        memory.store(02023, besm6_asm("xta 44,    asn 77")); // erase bit 48 from word 44b
        memory.store(02024, besm6_asm("asn 101,   atx 44")); // by doing << 1 >> 1
    }
    memory.store(02023 + 2 * system_load_list_flag,
                        besm6_asm("uj (17),       utc"));          // в статический загрузчик

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
    if (trace_enabled()) {
        std::cout << "------------------------------------------------------------\n";
    }
}

//
// Check for binary program (overlay).
//
bool Machine::is_overlay(const std::string &filename)
{
//std::cout << "--- file = '" << filename << "'\n";
    // Open text file.
    std::ifstream file(filename, std::ios_base::binary);
    if (!file.good()) {
        // Cannot open.
        return false;
    }

    // Check file size.
    file.seekg(0, std::ios_base::end);
    auto nbytes = file.tellg();
//std::cout << "--- nbytes = " << nbytes << '\n';
    if (nbytes / PAGE_NBYTES < 2 || nbytes % PAGE_NBYTES != 0) {
        // Must be a multiple of the page size.
        return false;
    }

    // Check a magic word OVERLA at fixed offset.
    // 2365 3105 2444 6101
    // 2365 3105 2444 6101   CODE: ,ISO, 6HOVERLA
    // 0x4f5645524c41
    std::string word(6, '\0');
    file.seekg(01762 * 6, std::ios_base::beg);
    if (!file.read(&word[0], word.size())) {
        return false;
    }
//std::cout << "--- magic = " << std::oct
//          << (int)(uint8_t)word[0] << '-'
//          << (int)(uint8_t)word[1] << '-'
//          << (int)(uint8_t)word[2] << '-'
//          << (int)(uint8_t)word[3] << '-'
//          << (int)(uint8_t)word[4] << '-'
//          << (int)(uint8_t)word[5]
//          << std::dec << "\n";
    if (word != "OVERLA") {
        // Wrong file format.
        return false;
    }

    // Check base address of the binary.
    file.seekg(6, std::ios_base::beg);
    if (!file.read(&word[0], word.size())) {
        return false;
    }
    unsigned address = (word[5] & 0377) | ((word[4] << 8) & 077400);
//std::cout << "--- spec = " << std::oct
//          << (int)(uint8_t)word[0] << '-'
//          << (int)(uint8_t)word[1] << '-'
//          << (int)(uint8_t)word[2] << '-'
//          << (int)(uint8_t)word[3] << '-'
//          << (int)(uint8_t)word[4] << '-'
//          << (int)(uint8_t)word[5]
//          << std::dec << "\n";
//std::cout << "--- base = 0" << std::oct << address << std::dec << '\n';
    if (address != 0770) {
        throw std::runtime_error("Overlay at wrong base address");
    }
    return true;
}

//
// Load binary program (overlay).
//
void Machine::boot_overlay(const std::string &filename, const std::string &path)
{
    // Mount tape 9/monsys as disk 30, read only.
    disk_search_path = path;
    disk_mount_readonly(030, TAPE_MONSYS);

    // Phys i/o: re-direct drum 021 to disk 030.
    map_drum_to_disk(021, 030);

    // Open overlay as disk 60, read only.
    file_paths.push_back(filename);
    file_mount(060, file_paths.size(), false);

    // clang-format off
    memory.store(02000, besm6_asm("*70 3000,      utc"));          // читаем таблицу резидентных программ для загрузчика
    memory.store(02001, besm6_asm("xta 76363,     atx 76100"));    // восстановим испорченный IОLISТ*
    memory.store(02002, besm6_asm("*70 3001,      utc"));          // пишем ТРП на барабан
    memory.store(02003, besm6_asm("*70 3002,      utc"));          // читаем пустой каталог временной библиотеки
    memory.store(02004, besm6_asm("*70 3003,      utc"));          // пишем на барабан
    memory.store(02005, besm6_asm("*70 3004,      utc"));          // вторая зона пустого каталога временной библиотеки
    memory.store(02006, besm6_asm("*70 3005,      utc"));          // пишем на барабан
    memory.store(02007, besm6_asm("*70 3006,      utc"));          // читаем резидент MONITOR*
    memory.store(02010, besm6_asm("*70 3007,      utc"));          // читаем каталог оверлея

    memory.store(02011, besm6_asm("vtm 53401(17), *70 717"));      // читаем статический загрузчик (infloa по адресу 0717)
    memory.store(02012, besm6_asm("ita 17,        atx 716"));      // устанавливаем aload по адресу 0716
    memory.store(02013, besm6_asm("xta 17,        aax 3010"));     // берём адрес "Свободно"
    memory.store(02014, besm6_asm("aox 3011,      atx 17"));       // устанавливаем на 01000
    memory.store(02015, besm6_asm("atx 772,       ati 15"));       // записываем в заголовок, ставим начало программы
    memory.store(02016, besm6_asm("xta 3012,      atx 512"));      // ставим inf0 для статического загрузчика
    memory.store(02017, besm6_asm("xta 76000,     atx 770"));      // берём имя оверлея, записываем в заголовок
    memory.store(02020, besm6_asm("uj (17),       utc"));          // уходим в статический загрузчик

    memory.store(03000, 0'4014'3700'0021'0201ul); // э70: читаем таблицу резидентных программ для загрузчика
    memory.store(03001, 0'0000'3700'0020'0000ul); // э70: пишем ТРП на барабан
    memory.store(03002, 0'0014'3700'0021'0007ul); // э70: читаем пустой каталог временной библиотеки
    memory.store(03003, 0'0000'3700'0021'0000ul); // э70: пишем на барабан
    memory.store(03004, 0'0014'3700'0021'0010ul); // э70: вторая зона пустого каталога временной библиотеки
    memory.store(03005, 0'0000'3700'0021'0001ul); // э70: пишем на барабан
    memory.store(03006, 0'0014'0000'0021'0035ul); // э70: читаем резидент MONITOR*
    memory.store(03007, 0'0010'3700'0060'0000ul); // э70: читаем каталог оверлея
    memory.store(03010, 0'7777'7777'7770'0000ul); // маска битов 48:16.
    memory.store(03011, 0'0000'0000'0000'1000ul); // начальный адрес оверлея
    memory.store(03012, 0'0010'0000'0060'0000ul); // inf0 для статического загрузчика
    // clang-format on

    cpu.set_pc(02000);
    if (trace_enabled()) {
        std::cout << "------------------------------------------------------------\n";
    }
}
