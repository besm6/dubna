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
#include <iostream>
#include "machine.h"

// Static fields.
bool Machine::verbose = false;
uint64_t Machine::simulated_instructions = 0;

// Limit of instructions, by default.
const uint64_t Machine::DEFAULT_LIMIT = 100ULL * 1000 * 1000 * 1000;

//
// Initialize the machine.
//
Machine::Machine(Memory &memory) :
    memory(memory),
    progress_time_last(std::chrono::steady_clock::now())
{
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
        auto delta = time_now - progress_time_last;
        auto sec = std::chrono::duration_cast<std::chrono::seconds>(delta).count();

        // Emit message every 5 seconds.
        if (sec >= 5) {
            std::cerr << "----- Progress " << simulated_instructions << " -----" << std::endl;
            progress_time_last = time_now;
        }
    }
}

//
// Run the machine until completion.
// Return `done' flag: true when everything finished.
//
bool Machine::run()
{
    bool done;

    do {
        is_halted = false;

        try {
            // Simulate one instruction.
            done = advance();

        } catch (std::exception &ex) {
            std::cout << "\nFATAL ERROR: " << ex.what() << std::endl;
            //TODO: dump_state();
            throw;
        }

        if (progress_message_enabled) {
            show_progress();
        }

        if (is_halted) {
            return false;
        }
    } while (!done);

    return true;
}

//
// Run one instruction.
// Return `done' flag: true when finished.
// Throw exception in case of fatal error.
//
bool Machine::advance()
{
    //TODO: simulate one instruction.

    return true;
}

//
// Load input file.
// Throw exception on failure.
//
void Machine::load(const std::string &filename)
{
    //TODO: load job file
}

//
// Load input job from stream.
//
void Machine::load(std::istream &input)
{
    //TODO: load job from stream
}
