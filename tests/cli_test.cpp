//
// Tests for command line interface.
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
#include <cstdio>
#include <fstream>
#include <sys/wait.h>

#include "util.h"

TEST(cli, usage)
{
    // Run simulator via shell.
    FILE *pipe = popen(BUILD_DIR "/dubna --help", "r");
    ASSERT_TRUE(pipe != nullptr);

    // Capture the output.
    std::string result = stream_contents(pipe);
    std::cout << result;

    // Check exit code.
    int exit_status = pclose(pipe);
    int exit_code   = WEXITSTATUS(exit_status);
    ASSERT_NE(exit_status, -1);
    ASSERT_EQ(exit_code, 0);

    // Check output.
    EXPECT_NE(result.find("Version"), std::string::npos);
    EXPECT_NE(result.find("Usage:"), std::string::npos);
    EXPECT_NE(result.find("Options:"), std::string::npos);
}

TEST(cli, version)
{
    // Run simulator via shell.
    FILE *pipe = popen(BUILD_DIR "/dubna --version", "r");
    ASSERT_TRUE(pipe != nullptr);

    // Capture the output.
    std::string result = stream_contents(pipe);
    std::cout << result;

    // Check exit code.
    int exit_status = pclose(pipe);
    int exit_code   = WEXITSTATUS(exit_status);
    ASSERT_NE(exit_status, -1);
    ASSERT_EQ(exit_code, 0);

    // Check output.
    EXPECT_NE(result.find("Version"), std::string::npos);
    EXPECT_NE(result.find("."), std::string::npos);
    EXPECT_NE(result.find("-"), std::string::npos);
}

TEST(cli, trace_end_file)
{
    std::string base_name      = get_test_name();
    std::string job_filename   = "cli_" + base_name + ".dub";
    std::string trace_filename = "cli_" + base_name + ".trace";
    std::string command_line   = BUILD_DIR "/dubna --trace=" + trace_filename + " --debug=e " + job_filename;

    create_file(job_filename,
                "*name empty\n"
                "*end file\n");

    // Set path to the disk images.
    EXPECT_EQ(setenv("BESM6_PATH", TEST_DIR "/../tapes", 1), 0);

    // Run simulator via shell.
    FILE *pipe = popen(command_line.c_str(), "r");
    ASSERT_TRUE(pipe != nullptr);

    // Capture the output as vector of strings.
    auto output = stream_contents(pipe);
    std::cout << output;

    // Check exit code.
    int exit_status = pclose(pipe);
    int exit_code   = WEXITSTATUS(exit_status);
    ASSERT_NE(exit_status, -1);
    ASSERT_EQ(exit_code, 0);

    // Read the trace.
    auto trace = file_contents_split(trace_filename);

    // Check output.
    ASSERT_GE(trace.size(), 4u);
    EXPECT_TRUE(starts_with(trace[0], "Dubna Simulator Version"));
    EXPECT_STREQ(trace[1].c_str(), "02010 R: 00 070 3002 *70 3002");
    EXPECT_STREQ(trace[2].c_str(), "      Drum 21 PhysRead [00000-00377] = Zone 1 Sector 2");
    EXPECT_STREQ(trace[trace.size() - 5].c_str(), "00427 L: 00 074 0000 *74");
}

TEST(cli, help_libs)
{
    // Set path to the disk images.
    EXPECT_EQ(setenv("BESM6_PATH", TEST_DIR "/../tapes", 1), 0);

    // Run simulator via shell.
    FILE *pipe = popen(BUILD_DIR "/dubna --help-libs", "r");
    ASSERT_TRUE(pipe != nullptr);

    // Capture the output.
    std::string result = stream_contents(pipe);
    std::cout << result;

    // Check exit code.
    int exit_status = pclose(pipe);
    int exit_code   = WEXITSTATUS(exit_status);
    ASSERT_NE(exit_status, -1);
    ASSERT_EQ(exit_code, 0);

    // Check output.
    const std::string expect = R"(Library        Tape         Zone
--------------------------------
*library:1     12/librar    0000
*library:2     37/librar    0000
*library:3     37/librar    0340
*library:5     37/librar    0545
*library:6     37/librar    0650
*library:7     37/librar    1061
*library:10    37/librar    1220
*library:11    37/librar    1370
*library:12    12/librar    0375
*library:21    9/monsys     0240
*library:22    9/monsys     0172
*library:23    9/monsys     0320
*library:24    9/monsys     0172
*library:25    12/librar    0640
*library:26    483/grafpr   0000
*library:27    12/librar    0600
)";
    EXPECT_EQ(result, expect);
}

//
// Run binary file created with *overlay.
// See source in examples/overlay/hello-algol.dub.
//
TEST(cli, overlay_exe)
{
    std::string command_line = BUILD_DIR "/dubna " TEST_DIR "/overlay.exe";

    // Set path to the disk images.
    EXPECT_EQ(setenv("BESM6_PATH", TEST_DIR "/../tapes", 1), 0);

    // Run simulator via shell.
    FILE *pipe = popen(command_line.c_str(), "r");
    ASSERT_TRUE(pipe != nullptr);

    // Capture the output as vector of strings.
    auto output = stream_contents(pipe);
    std::cout << output;

    // Check exit code.
    int exit_status = pclose(pipe);
    int exit_code   = WEXITSTATUS(exit_status);
    ASSERT_NE(exit_status, -1);
    ASSERT_EQ(exit_code, 0);

    EXPECT_EQ(output, " HELLO, ALGOL!\n");
}

//
// Run overlay which has shebang line prepended, like "#!/usr/bin/env dubna".
//
TEST(cli, shebang_exe)
{
    std::string command_line = BUILD_DIR "/dubna " TEST_DIR "/shebang.exe";

    // Set path to the disk images.
    EXPECT_EQ(setenv("BESM6_PATH", TEST_DIR "/../tapes", 1), 0);

    // Run simulator via shell.
    FILE *pipe = popen(command_line.c_str(), "r");
    ASSERT_TRUE(pipe != nullptr);

    // Capture the output as vector of strings.
    auto output = stream_contents(pipe);
    std::cout << output;

    // Check exit code.
    int exit_status = pclose(pipe);
    int exit_code   = WEXITSTATUS(exit_status);
    ASSERT_NE(exit_status, -1);
    ASSERT_EQ(exit_code, 0);

    EXPECT_EQ(output, " HELLO, ALGOL!\n");
}

//
// Run overlay with an input file argument.
//
TEST(cli, besmcat_exe)
{
    std::string command_line = BUILD_DIR "/dubna " TEST_DIR "/besmcat.exe besmcat.txt";

    // Set path to the disk images.
    EXPECT_EQ(setenv("BESM6_PATH", TEST_DIR "/../tapes", 1), 0);

    // Create input text file.
    const std::string contents = R"(the quick brown fox jumps over the lazy dog
съешь же ещё этих мягких французских булок, да выпей чаю
єхидна, ґава, їжак ще й шиплячі плазуни бігцем форсують янцзи
ах, чудна българска земьо, полюшвай цъфтящи жита
)";
    create_file("besmcat.txt", contents);

    // Run simulator via shell.
    FILE *pipe = popen(command_line.c_str(), "r");
    ASSERT_TRUE(pipe != nullptr);

    // Capture the output as vector of strings.
    auto output = stream_contents(pipe);
    std::cout << output;

    // Check exit code.
    int exit_status = pclose(pipe);
    int exit_code   = WEXITSTATUS(exit_status);
    ASSERT_NE(exit_status, -1);
    ASSERT_EQ(exit_code, 0);

    EXPECT_EQ(output, " THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG\n"
                      " CЪEШЬ ЖE EЩE ЭTИX MЯГKИX ФPAHЦYЗCKИX БYЛOK, ДA BЫПEЙ ЧAЮ\n"
                      " EXИДHA, ГABA, IЖAK ЩE Й ШИПЛЯЧI ПЛAЗYHИ БIГЦEM ФOPCYЮTЬ ЯHЦЗИ\n"
                      " AX, ЧYДHA БЪЛГAPCKA ЗEMЬO, ПOЛЮШBAЙ ЦЪФTЯЩИ ЖИTA\n");
}
