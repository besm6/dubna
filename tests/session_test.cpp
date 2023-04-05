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
TEST_F(dubna_session, e64_simple)
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
    auto output = run_job_and_capture_output(R"(*name print
*assem
 program: ,name,
          ,*64 , gost
          ,*64 , cmd
          ,*64 , octal
          ,*64 , real
          ,*64 , itm
          ,*64 , hex
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
 hex:     ,    , data
          ,    , data
        7 ,001 , 12         .hex numbers
        8 ,    ,
          ,end ,
*execute
*end file
)");

    auto expect = file_contents(TEST_DIR "/output_e64_simple.expect");
    check_output(output, expect);
}

TEST_F(dubna_session, e64_newline)
{
    auto output = run_job_and_capture_output(R"(*name print
*no list
*no load
*assem
 program: ,name,
          ,*64 , info
          ,*74 ,
 info:    ,    , text
          ,    , text
        0 ,    ,
        8 ,    ,
 text:    ,gost, 2h'214''231' . newline end-of-text
          ,end ,
*execute
*end file
)");

    // Isolated newline symbol.
    static const std::string expect =
        "";
    check_program_output(output, expect);
}

TEST_F(dubna_session, DISABLED_e64_newpage)
{
    auto output = run_job_and_capture_output(R"(*name print
*no list
*no load
*assem
 program: ,name,
          ,*64 , info
          ,*74 ,
 info:    ,    , text
          ,    , text
        0 ,    ,
        8 ,    ,
 text:    ,gost, 2h'201''231' . newpage end-of-text
          ,end ,
*execute
*end file
)");

    // Isolated newpage symbol.
    static const std::string expect =
        "\f ";
    check_program_output(output, expect);
}

TEST_F(dubna_session, e64_128chars)
{
    auto output = run_job_and_capture_output(R"(*name print
*no list
*no load
*assem
 program: ,name,
          ,*64 , info
          ,*74 ,
 info:    ,    , text
          ,    , text
        0 ,    ,
        8 ,    ,
 text:    ,gost, 30h--------10--------20--------30
          ,gost, 30h--------40--------50--------60
          ,gost, 30h--------70--------80--------90
          ,gost, 30h-------100-------110-------120
          ,gost, 9h12345678'231' . end-of-text
          ,end ,
*execute
*end file
)");

    // Exactly 128 characters in the line.
    // Newline is added implicitly.
    static const std::string expect =
        "--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678";
    check_program_output(output, expect);
}

TEST_F(dubna_session, e64_128chars_newline)
{
    auto output = run_job_and_capture_output(R"(*name print
*no list
*no load
*assem
 program: ,name,
          ,*64 , info
          ,*74 ,
 info:    ,    , text
          ,    , text
        0 ,    ,
        8 ,    ,
 text:    ,gost, 30h--------10--------20--------30
          ,gost, 30h--------40--------50--------60
          ,gost, 30h--------70--------80--------90
          ,gost, 30h-------100-------110-------120
          ,gost, 10h12345678'214''231' . newline end-of-text
          ,end ,
*execute
*end file
)");

    // Exactly 128 characters in the line.
    // Newline in position 129 is explicit in this case.
    static const std::string expect =
        "--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678";
    check_program_output(output, expect);
}

TEST_F(dubna_session, e64_128chars_newpage)
{
    auto output = run_job_and_capture_output(R"(*name print
*no list
*no load
*assem
 program: ,name,
          ,*64 , info
          ,*74 ,
 info:    ,    , text
          ,    , text
        0 ,    ,
        8 ,    ,
 text:    ,gost, 30h--------10--------20--------30
          ,gost, 30h--------40--------50--------60
          ,gost, 30h--------70--------80--------90
          ,gost, 30h-------100-------110-------120
          ,gost, 10h12345678'201''231' . newpage end-of-text
          ,end ,
*execute
*end file
)");

    // Exactly 128 characters in the line.
    // Newpage symbol in position 129 is ignored.
    static const std::string expect =
        "--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678";
    check_program_output(output, expect);
}

TEST_F(dubna_session, e64_129chars)
{
    auto output = run_job_and_capture_output(R"(*name print
*no list
*no load
*assem
 program: ,name,
          ,*64 , info
          ,*74 ,
 info:    ,    , text
          ,    , text
        0 ,    ,
        8 ,    ,
 text:    ,gost, 30h--------10--------20--------30
          ,gost, 30h--------40--------50--------60
          ,gost, 30h--------70--------80--------90
          ,gost, 30h-------100-------110-------120
          ,gost, 10h12345678x'231' . end-of-text
          ,end ,
*execute
*end file
)");

    // Exactly 128 characters in the line.
    // Newline is added implicitly before 'x'.
    static const std::string expect =
        "--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678\n"
        "X";
    check_program_output(output, expect);
}

TEST_F(dubna_session, e64_134chars)
{
    auto output = run_job_and_capture_output(R"(*name print
*no list
*no load
*assem
 program: ,name,
          ,*64 , info
          ,*74 ,
 info:    ,    , text
          ,    , text
        0 ,    ,
        8 ,    ,
 text:    ,gost, 30h--------10--------20--------30
          ,gost, 30h--------40--------50--------60
          ,gost, 30h--------70--------80--------90
          ,gost, 30h-------100-------110-------120
          ,gost, 15h12345678xabcde'231' . end-of-text
          ,end ,
*execute
*end file
)");

    // Exactly 128 characters in the line.
    // Newline is added implicitly before 'x'.
    static const std::string expect =
        "--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678\n"
        "XABCDE";
    check_program_output(output, expect);
}

TEST_F(dubna_session, e64_3chars_newline_3chars)
{
    auto output = run_job_and_capture_output(R"(*name print
*no list
*no load
*assem
 program: ,name,
          ,*64 , info
          ,*74 ,
 info:    ,    , text
          ,    , text
        0 ,    ,
        8 ,    ,
 text:    ,gost, 8hfoo'214'bar'231' . newline end-of-text
          ,end ,
*execute
*end file
)");

    // Newline in position 4 break the line.
    static const std::string expect =
        "FOO\n"
        "BAR";
    check_program_output(output, expect);
}

TEST_F(dubna_session, DISABLED_e64_3chars_newpage_3chars)
{
    auto output = run_job_and_capture_output(R"(*name print
*no list
*no load
*assem
 program: ,name,
          ,*64 , info
          ,*74 ,
 info:    ,    , text
          ,    , text
        0 ,    ,
        8 ,    ,
 text:    ,gost, 8hfoo'201'bar'231' . newpage end-of-text
          ,end ,
*execute
*end file
)");

    // Newpage in position 4 break the line.
    static const std::string expect =
        "\fFOO BAR";
    check_program_output(output, expect);
}

TEST_F(dubna_session, DISABLED_e64_128chars_newline_5chars)
{
    auto output = run_job_and_capture_output(R"(*name print
*no list
*no load
*assem
 program: ,name,
          ,*64 , info
          ,*74 ,
 info:    ,    , text
          ,    , text
        0 ,    ,
        8 ,    ,
 text:    ,gost, 30h--------10--------20--------30
          ,gost, 30h--------40--------50--------60
          ,gost, 30h--------70--------80--------90
          ,gost, 30h-------100-------110-------120
          ,gost, 15h12345678'214'abcde'231' . newline end-of-text
          ,end ,
*execute
*end file
)");

    // Exactly 128 characters in the line.
    // Newline in position 129 adds extra empty line.
    static const std::string expect =
        "--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678\n"
        "\n"
        "ABCDE";
    check_program_output(output, expect);
}

TEST_F(dubna_session, DISABLED_e64_128chars_newpage_5chars)
{
    auto output = run_job_and_capture_output(R"(*name print
*no list
*no load
*assem
 program: ,name,
          ,*64 , info
          ,*74 ,
 info:    ,    , text
          ,    , text
        0 ,    ,
        8 ,    ,
 text:    ,gost, 30h--------10--------20--------30
          ,gost, 30h--------40--------50--------60
          ,gost, 30h--------70--------80--------90
          ,gost, 30h-------100-------110-------120
          ,gost, 15h12345678'201'abcde'231' . newpage end-of-text
          ,end ,
*execute
*end file
)");

    // Exactly 128 characters in the line, terminated by \f instead of \n.
    // Extra space is added.
    // Newpage symbol in position 129 is ignored.
    static const std::string expect =
        "--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678\f ABCDE";
    check_program_output(output, expect);
}
