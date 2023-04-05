//
// Tests for extracode e64.
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

TEST_F(dubna_session, e64_abra_cadabra)
{
    auto output = run_job_and_capture_output(R"(*name print
*no list
*no load
*assem
 program: ,name,
          ,*64 , info1
          ,*64 , infoe
          ,*64 , info2
          ,*74 ,
 info1:   ,    , abra
          ,    , abra
        0 ,    ,
        8 ,    ,
 infoe:   ,    , empty
          ,    , empty
        0 ,    ,
        8 ,    ,
 info2:   ,    , cadabra
          ,    , cadabra
        0 ,    ,
        8 ,    ,
 abra:    ,gost, 5habra'231'
 empty:   ,gost, 1h'231'
 cadabra: ,gost, 8hcadabra'231'
          ,end ,
*execute
*end file
)");
    output = extract_after_execute(output);

    // Print four letters, then print seven letters.
    EXPECT_EQ(output, R"(*EXECUTE
ABRA
CADABRA
)");
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
    output = extract_after_execute(output);

    // Isolated newline symbol.
    EXPECT_EQ(output, R"(*EXECUTE

)");
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
    output = extract_after_execute(output);

    // Isolated newpage symbol.
    EXPECT_EQ(output, "*EXECUTE\f ");
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
    output = extract_after_execute(output);

    // Exactly 128 characters in the line.
    // Newline is added implicitly.
    EXPECT_EQ(output, R"(*EXECUTE
--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678
)");
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
    output = extract_after_execute(output);

    // Exactly 128 characters in the line.
    // Newline in position 129 is explicit in this case.
    EXPECT_EQ(output, R"(*EXECUTE
--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678
)");
}

TEST_F(dubna_session, DISABLED_e64_128chars_newpage)
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
    output = extract_after_execute(output);

    // Exactly 128 characters in the line.
    // Newpage symbol in position 129 is ignored.
    EXPECT_EQ(output, R"(*EXECUTE
--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678
)");
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
    output = extract_after_execute(output);

    // Exactly 128 characters in the line.
    // Newline is added implicitly before 'x'.
    EXPECT_EQ(output, R"(*EXECUTE
--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678
)");
}

TEST_F(dubna_session, DISABLED_e64_134chars)
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
    output = extract_after_execute(output);

    // Exactly 128 characters in the line.
    // Newline is added implicitly before 'x'.
    EXPECT_EQ(output, R"(*EXECUTE
--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678
XABCDE
)");
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
    output = extract_after_execute(output);

    // Newline in position 4 break the line.
    EXPECT_EQ(output, R"(*EXECUTE
FOO
BAR
)");
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
    output = extract_after_execute(output);

    // Newpage in position 4 break the line.
    EXPECT_EQ(output, R"(*EXECUTE
FOO BAR
)");
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
    output = extract_after_execute(output);

    // Exactly 128 characters in the line.
    // Newline in position 129 adds extra empty line.
    EXPECT_EQ(output, R"(*EXECUTE
--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678

ABCDE
)");
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
    output = extract_after_execute(output);

    // Exactly 128 characters in the line, terminated by \f instead of \n.
    // Extra space is added.
    // Newpage symbol in position 129 is ignored.
    EXPECT_EQ(output, R"(*EXECUTE
--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678 ABCDE
)");
}
