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
    EXPECT_STREQ(trace[trace.size() - 5].c_str(), "00427 L: 00 074 0000 *74");
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
    auto expect = file_contents(TEST_DIR "/expect_okno.txt");
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
    auto expect = file_contents(TEST_DIR "/expect_edit.txt");
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
    auto expect = file_contents(TEST_DIR "/expect_assem.txt");
    check_output(output, expect);
}

//
// Run *MADLEN example and check output.
//
TEST_F(dubna_session, madlen)
{
    auto output = run_job_and_capture_output(R"(*name мадлен
*madlen
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
    auto expect = file_contents(TEST_DIR "/expect_madlen.txt");
    check_output(output, expect);
}

//
// Run *BEMSH example and check output.
//
TEST_F(dubna_session, bemsh)
{
    auto output = run_job_and_capture_output(R"(*name бемш
*bemsh
ввд$$$
prog    старт   512
        э64     инф64
        пб      (13)
инф64   мода    текст
        мода    текст
        конк    к'000010000'
        конк    к'100000000'
текст   текст   п'Приветик! Это автокод БЕМШ.'
        конд    м40b'231'
        финиш
квч$$$
трн$$$
0-0
блмак
бтмалф
кнц$$$
*main prog
*execute
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_bemsh.txt");
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
    auto expect = file_contents(TEST_DIR "/expect_fortran.txt");
    check_output(output, expect);
}

//
// Run *FTN example and check output.
//
TEST_F(dubna_session, ftn)
{
    auto output = run_job_and_capture_output(R"(*name фортран
*ftn
        program hello
        print 1000
        stop
 1000   format('Hello, World!')
        end
*execute
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_ftn.txt");
    check_output(output, expect);
}

//
// Run *FOREX example and check output.
//
TEST_F(dubna_session, forex)
{
    auto output = run_job_and_capture_output(R"(*name форекс
*forex
        program hello
        print 1000
        stop
 1000   format('Hello, World!')
        end
*execute
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_forex.txt");
    check_output(output, expect);
}

//
// Run *ALGOL example and check output.
//
TEST_F(dubna_session, algol)
{
    auto output = run_job_and_capture_output(R"(*name aлгол
*algol
'begin'
    print(''Hello, World!'');
'end'
'eop'
*execute
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_algol.txt");
    check_output(output, expect);
}

//
// Run *PASCAL example and check output.
//
TEST_F(dubna_session, pascal)
{
    auto output = run_job_and_capture_output(R"(*name pascal
*pascal
program main (output);
_(
    writeln('Hello, World!');
_).
*library:22
*execute
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_pascal.txt");
    check_output(output, expect);
}

//
// Run *LIBPUNCH example and check output.
//
TEST_F(dubna_session, libpunch)
{
    // See Mazny book, page 140.
    auto output = run_job_and_capture_output(R"(*name libpunch
*table:libpunch(print8)
*libpunch
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_libpunch.txt");
    check_output(output, expect);
}

//
// Print real values from Fortran and check output.
// https://github.com/besm6/dubna/issues/1
//
TEST_F(dubna_session, epsilon)
{
    auto output = run_job_and_capture_output(R"(*name epsilon
*no list
      program eps
      real a
      a = 4020000000000010b
      print 1000, a, a
      a = 4020000000000004b
      print 1000, a, a
      a = 4020000000000002b
      print 1000, a, a
      a = 4020000000000001b
      print 1000, a, a
 1000 format(o17, f44.40)
      stop
      end
*no load list
*execute
*end file
)");
    // Split output into lines.
    auto lines = multiline_split(output);
    auto len = lines.size();
    ASSERT_GE(lines.size(), 8);

    //
    // This example shows bugs in floating-point formatted print of Fortran-Dubna.
    //

    // This line is OK.
    EXPECT_STREQ(lines[len-8].c_str(), " 4020000000000010 -0.9999999999927240423858165740966796875000");

    // Correct output would be: " 4020000000000004 -0.9999999999963620211929082870483398437500"
    EXPECT_STREQ(lines[len-7].c_str(), " 4020000000000004 -0.9999999999956344254314899444580078125000");

    // Correct output would be: " 4020000000000002 -0.9999999999981810105964541435241699218750"
    EXPECT_STREQ(lines[len-6].c_str(), " 4020000000000002 -0.9999999999970896169543266296386718750000");

    // Correct output would be: " 4020000000000001 -0.9999999999990905052982270717620849609375"
    EXPECT_STREQ(lines[len-5].c_str(), " 4020000000000001 -0.9999999999985448084771633148193359375000");
}

//
// Capture overflow.
// https://github.com/besm6/dubna/issues/2
//
TEST_F(dubna_session, overflow)
{
    auto output = run_job_and_capture_output(R"(*name overflow
*no list
*fortran
       program ovfl
       a = 1.0
       i = 0
       if (ifovfl(0) .eq. 1) goto 10
 20    a = a + a
       i = i + 1
       goto 20
 10    print 1000, i
 1000  format (i6)
       end
*no load list
*execute
*end file
)");
    // Split output into lines.
    auto lines = multiline_split(output);
    auto len = lines.size();
    ASSERT_GE(lines.size(), 5);

    EXPECT_STREQ(lines[len-5].c_str(), "    62");
}

//
// Capture division by zero.
//
TEST_F(dubna_session, divzero)
{
    auto output = run_job_and_capture_output(R"(*name divzero
*no list
*fortran
       program divz
       a = 123.4
       i = 0
       if (ifovfl(0) .eq. 1) goto 10
 20    a = a / i
       i = i + 1
       goto 20
 10    print 1000, a
 1000  format (f6.1)
       end
*no load list
*execute
*end file
)");
    // Split output into lines.
    auto lines = multiline_split(output);
    auto len = lines.size();
    ASSERT_GE(lines.size(), 5);

    EXPECT_STREQ(lines[len-5].c_str(), " 123.4");
}

//
// Print contents of default library.
//
TEST_F(dubna_session, pcatalog)
{
    auto output = run_job_and_capture_output(R"(*name постоян.библиотека
*call pcatalog
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_pcatalog.txt");
    check_output(output, expect);
}

//
// Print a list of system overlays.
//
TEST_F(dubna_session, sovcatal)
{
    auto output = run_job_and_capture_output(R"(*name системные оверлеи
*call sovcatal
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_sovcatal.txt");
    check_output(output, expect);
}

//
// Print contents of library 1.
//
TEST_F(dubna_session, library1)
{
    auto output = run_job_and_capture_output(R"(*name библиотека 1
*tape:12/librar,32
*call setftn:one,long
*call librefer:tape=320000
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_library1.txt");
    check_output(output, expect);
}

//
// Print contents of library 2.
//
TEST_F(dubna_session, library2)
{
    auto output = run_job_and_capture_output(R"(*name библиотека 2
*tape:37/librar,32
*call setftn:one,long
*call librefer:tape=320000
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_library2.txt");
    check_output(output, expect);
}

//
// Print contents of library 3.
//
TEST_F(dubna_session, library3)
{
    auto output = run_job_and_capture_output(R"(*name библиотека 3
*tape:37/librar,32
*call setftn:one,long
*call librefer:tape=320340
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_library3.txt");
    check_output(output, expect);
}

//
// Print contents of library 5.
//
TEST_F(dubna_session, library5)
{
    auto output = run_job_and_capture_output(R"(*name библиотека 5
*tape:37/librar,32
*call setftn:one,long
*call librefer:tape=320545
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_library5.txt");
    check_output(output, expect);
}

//
// Print contents of library 12.
//
TEST_F(dubna_session, library12)
{
    auto output = run_job_and_capture_output(R"(*name библиотека 12
*tape:12/librar,32
*call setftn:one,long
*call librefer:tape=320375
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_library12.txt");
    check_output(output, expect);
}

//
// Print contents of library 21.
//
TEST_F(dubna_session, library21)
{
    auto output = run_job_and_capture_output(R"(*name библиотека 21
*tape:9/monsys,32
*call setftn:one,long
*call librefer:tape=320240
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_library21.txt");
    check_output(output, expect);
}

//
// Print contents of library 22.
//
TEST_F(dubna_session, library22)
{
    auto output = run_job_and_capture_output(R"(*name библиотека 22
*tape:9/monsys,32
*call setftn:one,long
*call librefer:tape=320172
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_library22.txt");
    check_output(output, expect);
}

//
// Print contents of library 23.
//
TEST_F(dubna_session, library23)
{
    auto output = run_job_and_capture_output(R"(*name библиотека 23
*tape:9/monsys,32
*call setftn:one,long
*call librefer:tape=320320
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_library23.txt");
    check_output(output, expect);
}

//
// Run ASPID example and check output.
//
TEST_F(dubna_session, aspid)
{
    auto output = run_job_and_capture_output(R"(*name аспид
*library:22,23
*call aspid*d
- А поворотись-ка, сын! Экой ты смешной какой!
Что это на вас за поповские подрясники?
И этак все ходят в академии? - Такими словами встретил
старый Бульба двух сыновей своих, учившихся в киевской
бурсе и приехавших домой к отцу.
%KP
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_aspid.txt");
    check_output(output, expect);
}

//
// Run WHATIS to show contents of tape MONSYS.
//
TEST_F(dubna_session, whatis_monsys)
{
    auto output = run_job_and_capture_output(R"(*name whatis
*tape:9/monsys,31
*library:23
*call whatis
3100000437
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_whatis_monsys.txt");
    check_output(output, expect);
}

//
// Use DTRAN to disassemble STOP* routine.
//
TEST_F(dubna_session, dtran)
{
    auto output = run_job_and_capture_output(R"(*name dtran
*library:23
*call dtran(stop*)
*assem
*read:1
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_dtran.txt");
    check_output(output, expect);
}

//
// Mount tape 12/librar.
//
TEST_F(dubna_session, tape12)
{
    auto output = run_job_and_capture_output(R"(*name заказ ленты 12
*tape:12/librar,32
*library:23
*call whatis
3200000477
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_tape12.txt");
    check_output(output, expect);
}

//
// Mount tape 37/librar.
//
TEST_F(dubna_session, tape37)
{
    auto output = run_job_and_capture_output(R"(*name заказ ленты 37
*tape:37/librar,37
*library:23
*call whatis
3700000647
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_tape37.txt");
    check_output(output, expect);
}

//
// Show instructions for library 1.
//
TEST_F(dubna_session, lib1)
{
    auto output = run_job_and_capture_output(R"(*name библиотека 1
*library:1
*main lib1
*no load list
*execute
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_lib1.txt");
    check_output(output, expect);
}

//
// Show instructions for library 2.
//
TEST_F(dubna_session, lib2)
{
    auto output = run_job_and_capture_output(R"(*name библиотека 2
*library:2
*main lib2
*no load list
*execute
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_lib2.txt");
    check_output(output, expect);
}

//
// Show instructions for library 3.
//
TEST_F(dubna_session, lib3)
{
    auto output = run_job_and_capture_output(R"(*name библиотека 3
*library:3
*main alglib
*no load list
*execute
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_lib3.txt");
    check_output(output, expect);
}

//
// Draw Grafor picture on Watanabe plotter.
//
TEST_F(dubna_session, grafor_watanabe)
{
    auto output = run_job_and_capture_output(R"(*name графор
*call plotter:wx4675,direct
*ftn
        program grafor
        real x(100), y(100), z(100)
        x(1) = -1.9
        y(1) = sin(x(1))
        z(1) = cos(x(1)) * 1.5
        do 5 i = 2, 90
            x(i) = x(i-1) + 0.1
            y(i) = sin(x(i))
            z(i) = cos(x(i)) * 1.5
  5         continue
        call getpen(2)
        call page(15., 20., 'PAGE', 4, 1)
        call getpen(3)
        call limits(-2., 7., -1.5, 1.5)
        call region(3., 3., 10., 15., 'REGION', 6, 1)
        call getpen(6)
        call axes('X axis', 6, 1., 5, 'Y axis', 6, 0.3, 4, 0)
        call getpen(5)
        call linemo(x, y, 85, 2, 10)
        call broken(0.5, 0.2, 0.3, 0.2)
        call brline(x, z, 85)
        call endpg(0)
        end
*execute
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_grafor_watanabe.txt");
    check_output(output, expect);

    // Check plotter output.
    auto watanabe = file_contents("watanabe.out");
    expect = file_contents(TEST_DIR "/expect_watanabe_output.txt");
    check_contents(watanabe, expect);

    // Check SVG output.
    watanabe = file_contents("watanabe.svg");
    expect = file_contents(TEST_DIR "/expect_watanabe.svg");
    EXPECT_EQ(watanabe, expect);
}

//
// Draw Grafor picture on Tektronix plotter.
//
TEST_F(dubna_session, grafor_tektronix)
{
    auto output = run_job_and_capture_output(R"(*name графор
*call plotter:tektronix,direct
*ftn
        program grafor
        call page(14.0, 10.0, 'PAGE', 4, 0)
        call limits(0.0, 14.0, 0.0, 10.0)
        call symbol(0.5, 4.0, 2.5, 'Tektronix', 9, 0.0)
        call endpg(0)
        end
*execute
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_grafor_tektronix.txt");
    check_output(output, expect);

    // Check plotter output.
    auto tektronix = file_contents("tektronix.out");
    expect = file_contents(TEST_DIR "/expect_tektronix_output.bin");
    EXPECT_EQ(tektronix, expect);

    // Check SVG output.
    tektronix = file_contents("tektronix.svg");
    expect = file_contents(TEST_DIR "/expect_tektronix.svg");
    EXPECT_EQ(tektronix, expect);
}

//
// Draw Grafor picture on Calcomp plotter.
//
TEST_F(dubna_session, grafor_calcomp)
{
    auto output = run_job_and_capture_output(R"(*name графор
*call plotter:calcomp,direct
*ftn
        program grafor
        call page(14.0, 10.0, 'PAGE', 4, 0)
        call limits(0.0, 14.0, 0.0, 10.0)
        call symbol(0.1, 3.5, 3.3, 'Calcomp', 7, 0.0)
        call endpg(0)
        end
*execute
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_grafor_calcomp.txt");
    check_output(output, expect);

    // Check plotter output.
    auto calcomp = file_contents("calcomp.out");
    expect = file_contents(TEST_DIR "/expect_calcomp_output.bin");
    EXPECT_EQ(calcomp, expect);

    // Check SVG output.
    calcomp = file_contents("calcomp.svg");
    expect = file_contents(TEST_DIR "/expect_calcomp.svg");
    EXPECT_EQ(calcomp, expect);
}
