//
// Tests for Session class.
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
#include <fstream>

#include "fixture_session.h"

//
// Check the version string.
//
TEST_F(dubna_session, version)
{
    // Capture the usage message.
    std::string result = Session::get_version();
    std::cout << result;

    // Check output.
    EXPECT_NE(result.find("."), std::string::npos);
    EXPECT_NE(result.find("-"), std::string::npos);
}

//
// Enable tracing of extracodes and run empty job.
// Make sure the trace is correct.
//
TEST_F(dubna_session, trace_end_file)
{
    std::string base_name      = get_test_name();
    std::string job_filename   = base_name + ".dub";
    std::string trace_filename = base_name + ".trace";

    // Enable trace.
    session->set_trace_file(trace_filename.c_str(), "e");

    // Run the job.
    create_file(job_filename,
                "*name empty\n"
                "*end file\n");
    session->set_job_file(job_filename);
    session->run();

    // Read the trace.
    auto trace = file_contents_split(trace_filename);

    // Check output.
    ASSERT_GE(trace.size(), 4);
    EXPECT_TRUE(starts_with(trace[0], "Dubna Simulator Version"));
    EXPECT_STREQ(trace[1].c_str(), "02010 R: 00 070 3002 *70 3002");
    EXPECT_STREQ(trace[2].c_str(), "      Drum 21 PhysRead [00000-00377] = Zone 1 Sector 2");
    EXPECT_STREQ(trace[trace.size() - 5].c_str(), "00020 L: 00 074 0000 *74");
}

//
// Run 'OKHO' example and check output.
//
TEST_F(dubna_session, okno)
{
    auto output = run_job_and_capture_output(R"(*name окно
*call ОКНО
*call ВОКНО
*end file
)");
    auto expect = file_contents(TEST_DIR "/output_okno.expect");
    check_output(output, expect);
}

//
// Run *EDIT example and check output.
//
TEST_F(dubna_session, edit)
{
    auto output = run_job_and_capture_output(R"(*name Edit
*edit
*RO
*W:27001,2
Варкалось. Хливкие шорьки
Пырялись по наве,
И хрюкотали зелюки,
Как мюмзики в мове.
*EE
*
*edit
*R:270001
*LL
*EE
*end file
)");
    auto expect = file_contents(TEST_DIR "/output_edit.expect");
    check_output(output, expect);
}

//
// Run *ASSEM example and check output.
//
TEST_F(dubna_session, assem)
{
    auto output = run_job_and_capture_output(R"(*name ассемблер
*assem
 program: ,name,
          ,*64 , inf64
          ,*74 ,
 inf64:   ,    , text
          ,    , text
          ,001 ,
         8,    ,
 text:    ,gost, 18h Hello, World!'214'
          ,gost, 6h'231'
          ,end ,
*execute
*end file
)");
    auto expect = file_contents(TEST_DIR "/output_assem.expect");
    check_output(output, expect);
}

//
// Run *FORTRAN example and check output.
//
TEST_F(dubna_session, fortran)
{
    auto output = run_job_and_capture_output(R"(*name фортран
*fortran
        program hello
        print 1000
        stop
 1000   format('Hello, World!')
        end
*execute
*end file
)");
    auto expect = file_contents(TEST_DIR "/output_fortran.expect");
    check_output(output, expect);
}

//
// Try all formats of extracode 064.
//
TEST_F(dubna_session, e64_trivial)
{
    // Format of e64 info:
    //
    //      format, offset, digits
    //      f+skip, width , repeat
    //
    // Here:
    //      format - Format of the output: 0-textGost, 1-instruction, 2-octal, 3-real, 4-textITM
    //      offset - Position at which to start (A)
    //      digits - Number of digits (L) to print
    //      f=8    - Finish on this item
    //      skip   - Number of extra empty lines at the end
    //      width  - Interval between elements
    //      repeat - Repeat count (K) minus 1
    //
    auto output = run_job_and_capture_output(R"(*name print trivial
*assem
 program: ,name,
          ,*64 , gost
          ,*64 , cmd
          ,*64 , octal
          ,*64 , real
          ,*64 , itm
          ,*74 ,
 gost:    ,    , text
          ,    , text
        0 ,001 ,            .text in Gost encoding
        8 ,    ,
 text:    ,gost, 18hGOST encoding'231'
 cmd:     ,    , code
          ,    , code
        1 ,001 ,            .instruction code
        8 ,    ,
 code:  12, 34 , 56701b
        02,043 , 76543b
 octal:   ,    , data
          ,    , data
        2 ,001 , 12         .octal numbers
        8 ,    ,
 data:    ,oct , 1234 5670 7654 3210
 real:    ,    , values
          ,    , values
        3 ,001 , 20         .real numbers
        8 ,    ,
 values:  ,real, 3.141592653589793238
 itm:     ,    , textitm
          ,    , textitm
        4 ,001 ,            .text in ITM encoding
        8 ,    ,
 textitm: ,oct , 6310 0607 0371 0206
          ,oct , 4350 1622 4310 3213
          ,oct , 3000 0000 0000 0000
          ,end ,
*execute
*end file
)");

    auto expect = file_contents(TEST_DIR "/output_e64_trivial.expect");
    check_output(output, expect);
}
