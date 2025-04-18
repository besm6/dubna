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

    auto expect = file_contents(TEST_DIR "/expect_e64_simple.txt");
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

TEST_F(dubna_session, e64_newpage)
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
    EXPECT_EQ(output, "*EXECUTE\f\n");
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
    // Newline at position 129 is explicit in this case.
    EXPECT_EQ(output, R"(*EXECUTE
--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678
)");
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
    output = extract_after_execute(output);

    // Exactly 128 characters in the line.
    // Newpage symbol at position 129 is ignored.
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

    // Newline at position 4 break the line.
    EXPECT_EQ(output, R"(*EXECUTE
FOO
BAR
)");
}

TEST_F(dubna_session, e64_3chars_newpage_3chars)
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

    // Newpage at position 4 break the line.
    EXPECT_EQ(output, "*EXECUTE\fFOO BAR\n");
}

TEST_F(dubna_session, e64_128chars_newline_5chars)
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
    // Newline at position 129 adds extra empty line.
    EXPECT_EQ(output, R"(*EXECUTE
--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678

ABCDE
)");
}

TEST_F(dubna_session, e64_128chars_newpage_5chars)
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
    // Newpage symbol at position 129 is ignored.
    EXPECT_EQ(output, R"(*EXECUTE
--------10--------20--------30--------40--------50--------60--------70--------80--------90-------100-------110-------12012345678 ABCDE
)");
}

TEST_F(dubna_session, e64_setpos0)
{
    auto output = run_job_and_capture_output(R"(*name print
*no list
*no load
*assem
 program: ,name,
          ,*64 , line1
          ,*64 , line2
          ,*74 ,
 line1:   ,    , text1
          ,    , text1
        0 ,    ,
        8 ,    ,
 line2:   ,    , text2
          ,    , text2
        0 ,    ,
        8 ,    ,
 text1:   ,gost, 5h  --'231'
 text2:   ,gost, 30h       here'200''0'foobar'231'
          ,end ,
*execute
*end file
)");

    // Set position to column #0.
    auto expect = R"(*EXECUTE
  --
FOOBAR HERE
)";
    output = extract_after_execute(output);
    EXPECT_EQ(output, expect);
}

TEST_F(dubna_session, e64_overprint_dispak)
{
    auto output = run_job_and_capture_output(R"(*name print
*no list
*no load
*assem
 program: ,name,
          ,*64 , line1
          ,*64 , line2
          ,*74 ,
 line1:   ,    , text1
          ,    , text1
        0 ,    ,
        8 ,    ,
 line2:   ,    , text2
          ,    , text2
        0 ,    ,
        8 ,    ,
 text1:   ,gost, 5h  --'172'        . end-of-info
 text2:   ,gost, 8hf'212'oobar'172' . overprint end-of-info
          ,end ,
*execute
*end file
)");

    // Overprint at position 2, according to Dispak documentation.
    auto expect = R"(*EXECUTE
F --\
  OOBAR
)";
    output = extract_after_execute(output);
    EXPECT_EQ(output, expect);
}

TEST_F(dubna_session, e64_overprint_dubna)
{
    auto output = run_job_and_capture_output(R"(*name print
*no list
*no load
*assem
 program: ,name,
          ,*64 , line1
          ,*64 , line2
          ,*74 ,
 line1:   ,    , text1
          ,    , text1
        0 ,    ,
        8 ,    ,
 line2:   ,    , text2
          ,    , text2
        0 ,    ,
        8 ,    ,
 text1:   ,gost, 5h  --'231'        . end-of-text
 text2:   ,gost, 8h'212'foobar'231' . overprint end-of-text
          ,end ,
*execute
*end file
)");

    // Overprint at position 1.
    auto expect = R"(*EXECUTE
 F--\
  OOBAR
)";
    output = extract_after_execute(output);
    EXPECT_EQ(output, expect);
}


TEST_F(dubna_session, e64_overprint_fortran)
{
    auto output = run_job_and_capture_output(R"(*name overprint
*no li
*no lo
      program main
      print 1
      print 2
      print 3
      print 4
      print 5
      print 6
   1  format('  A')
   2  format ('+ B')
   3  format ('+  C')
   4  format ('+   D')
   5  format ('+    E')
   6  format ('+     F')
      end
*execute
*end file
)");

    // No need for overprint, all characters must be on one line.
    auto expect = R"(*EXECUTE
  ABCDEF
)";
    output = extract_after_execute(output);
    EXPECT_EQ(output, expect);
}

TEST_F(dubna_session, e64_repeat0)
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
 text:    ,gost, 6h/*'174''0'/'175' . eoln 0
          ,gost, 6h/*'265''0'/'175' . repeat 0
          ,gost, 1h'377'
          ,end ,
*execute
*end file
)");
    output = extract_after_execute(output);

    // Repeat symbol 0 times.
    EXPECT_EQ(output, R"(*EXECUTE
/*/
/*/
)");
}

TEST_F(dubna_session, e64_repeat1)
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
 text:    ,gost, 6h/*'174''1'/'175' . eoln 1
          ,gost, 6h/*'265''1'/'175' . repeat 1
          ,gost, 1h'377'
          ,end ,
*execute
*end file
)");
    output = extract_after_execute(output);

    // Repeat symbol 1 times.
    EXPECT_EQ(output, R"(*EXECUTE
/**/
/**/
)");
}

TEST_F(dubna_session, e64_repeat7)
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
 text:    ,gost, 6h/*'174''7'/'175' . eoln 7
          ,gost, 6h/*'265''7'/'175' . repeat 7
          ,gost, 1h'377'
          ,end ,
*execute
*end file
)");
    output = extract_after_execute(output);

    // Repeat symbol 7 times.
    EXPECT_EQ(output, R"(*EXECUTE
/********/
/********/
)");
}

TEST_F(dubna_session, e64_repeat31)
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
 text:    ,gost, 6h/*'174''37'/'175' . eoln 31
          ,gost, 6h/*'265''37'/'175' . repeat 31
          ,gost, 1h'377'
          ,end ,
*execute
*end file
)");
    output = extract_after_execute(output);

    // Repeat symbol 31 times.
    EXPECT_EQ(output, R"(*EXECUTE
/********************************/
/********************************/
)");
}

TEST_F(dubna_session, e64_repeat124)
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
 text:    ,gost, 6h/*'174''174'/'175' . eoln 124
          ,gost, 6h/*'265''174'/'175' . repeat 124
          ,gost, 1h'377'
          ,end ,
*execute
*end file
)");
    output = extract_after_execute(output);

    // Repeat symbol 124 times.
    EXPECT_EQ(output, R"(*EXECUTE
/*****************************************************************************************************************************/
/*****************************************************************************************************************************/
)");
}

TEST_F(dubna_session, e64_repeat125)
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
 text:    ,gost, 6h/*'174''175'/'175' . eoln 125
          ,gost, 6h/*'265''175'/'175' . repeat 125
          ,gost, 1h'377'
          ,end ,
*execute
*end file
)");
    output = extract_after_execute(output);

    // Repeat symbol 125 times.
    EXPECT_EQ(output, R"(*EXECUTE
/******************************************************************************************************************************/

/******************************************************************************************************************************/
)");
}

TEST_F(dubna_session, e64_repeat126)
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
 text:    ,gost, 6h/*'174''176'/'175' . eoln 126
          ,gost, 6h/*'265''176'/'175' . repeat 126
          ,gost, 1h'377'
          ,end ,
*execute
*end file
)");
    output = extract_after_execute(output);

    // Repeat symbol 126 times.
    EXPECT_EQ(output, R"(*EXECUTE
/*******************************************************************************************************************************
/
/*******************************************************************************************************************************
/
)");
}

TEST_F(dubna_session, e64_repeat127)
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
 text:    ,gost, 6h/*'174''177'/'175' . eoln 127
          ,gost, 6h/*'265''177'/'175' . repeat 127
          ,gost, 1h'377'
          ,end ,
*execute
*end file
)");
    output = extract_after_execute(output);

    // Repeat symbol 127 times.
    EXPECT_EQ(output, R"(*EXECUTE
/*******************************************************************************************************************************
*/
/*******************************************************************************************************************************
*/
)");
}

TEST_F(dubna_session, e64_repeat255)
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
 text:    ,gost, 6h/*'174''377'/'175' . eoln 255
          ,gost, 6h/*'265''377'/'175' . repeat 255
          ,gost, 1h'377'
          ,end ,
*execute
*end file
)");
    output = extract_after_execute(output);

    // Repeat symbol 255 times.
    EXPECT_EQ(output, R"(*EXECUTE
/*******************************************************************************************************************************
********************************************************************************************************************************
*/
/*******************************************************************************************************************************
********************************************************************************************************************************
*/
)");
}

TEST_F(dubna_session, boldprint)
{
    auto output = run_job_and_capture_output(R"(*name жирпеч
*ftn
      program main
      call жиpпeч('однажды в студёную зимнюю пору', 30, 1)
      call жиpпeч('сижу за решёткой в темнице сырой', 32, 5)
      call жиpпeч('гляжу поднимается медленно в гору', 33, 2)
      call жиpпeч('вскормлённый в неволе орёл молодой', 34, 6)
      stop
      end
*library:23
*execute
*end file
)");
    const std::string expect = R"(*EXECUTE
      *LIBRA:23 = ЖИPПEЧ
≠
         MAIN       01000                ЖИPПEЧ     01060                STOP*      01263                *ЖПPOT*  C 01312
         PROGRAM  E 01000                ЖИPЛИC   E 01166                EXIT     E 01264                CBOБOДHO   01313
≠
 OДHAЖДЫ B CTYДEHYЮ ЗИMHЮЮ ПOPY
     CИЖY ЗA PEШETKOЙ B TEMHИЦE CЫPOЙ
  ГЛЯЖY ПOДHИMAETCЯ MEДЛEHHO B ГOPY
      BCKOPMЛEHHЫЙ B HEBOЛE OPEЛ MOЛOДOЙ
)";
    output = extract_after_execute(output);
    EXPECT_EQ(output, expect);
}

TEST_F(dubna_session, e64_real_3)
{
    auto output = run_job_and_capture_output(R"(*name e64 real
*assembler
 program:   ,name,
            ,*64,   prt
            ,*74,
C variables
 s:         ,real,  12.0
            ,real,  13.0
            ,real,  14.0
C format for *64
 prt:   0   ,z00,   s   . start
        0   ,z00,   s+2 . end
        3   ,z00,   10  . float
        8   ,z00,   0   . final word
            ,end,
*execute
*end file
)");
    const std::string expect = R"(*EXECUTE
≠
         PROGRAM    01000                CBOБOДHO   01007
≠
 +120000⏨+02
 +130000⏨+02
 +140000⏨+02
)";
    output = extract_after_execute(output);
    EXPECT_EQ(output, expect);
}

TEST_F(dubna_session, DISABLED_e64_octal_3) // TODO
{
    auto output = run_job_and_capture_output(R"(*name e64 octal
*assembler
 program:   ,name,
            ,*64,   prt
            ,*74,
 s:         ,log,  12345
            ,log,  23456
            ,log,  34567
 prt:   0   ,z00,   s   . start
        0   ,z00,   s+2 . end
        2   ,z00,   10  . octal
        8   ,z00,   0   . final word
            ,end,
*execute
*end file
)");
    const std::string expect = R"(*EXECUTE
≠
         PROGRAM    01000                CBOБOДHO   01007
≠
0000012345
0000023456
0000034567
)";
    output = extract_after_execute(output);
    EXPECT_EQ(output, expect);
}

TEST_F(dubna_session, e64_hex_3)
{
    auto output = run_job_and_capture_output(R"(*name e64 hex
*assembler
 program:   ,name,
            ,*64,   prt
            ,*74,
 s:         ,log,  12345
            ,log,  23456
            ,log,  34567
 prt:   0   ,z00,   s   . start
        0   ,z00,   s+2 . end
        6   ,z00,   10  . hexadecimal
        8   ,z00,   0   . final word
            ,end,
*execute
*end file
)");
    const std::string expect = R"(*EXECUTE
≠
         PROGRAM    01000                CBOБOДHO   01007
≠
00000014E5
000000272E
0000003977
)";
    output = extract_after_execute(output);
    EXPECT_EQ(output, expect);
}

TEST_F(dubna_session, e64_instructions_3)
{
    auto output = run_job_and_capture_output(R"(*name e64 instructions
*assembler
 program:   ,name,
            ,*64,   prt
            ,*74,
 s:         ,log,   2566 0370 3364 4043
            ,log,   4027 3734 4476 5141
            ,log,   6010 3764 7035 1336
 prt:   0   ,z00,   s   . start
        0   ,z00,   s+2 . end
        1   ,z00,   10  . instructions
        8   ,z00,   0   . final word
            ,end,
*execute
*end file
)");
    const std::string expect = R"(*EXECUTE
≠
         PROGRAM    01000                CBOБOДHO   01007
≠
05 166 0370 06 36 44043
10 027 3734 11 076 5141
14 010 3764 16 035 1336
)";
    output = extract_after_execute(output);
    EXPECT_EQ(output, expect);
}
