//
// Check extracodes of elementary functions.
//
// Copyright (c) 2024 Serge Vakulenko
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

TEST_F(dubna_session, elem_func)
{
    auto output = run_job_and_capture_output(R"(*name элем. функции
*no list
*ftn
        program main
        real dsqrt(5)
        data dsqrt/2.0, 99999.0, 1.01, 0.99, 0.0003/
        real dsin(8)
        data dsin/0.0001, -0.0001, 0.7854, -0.7854,
     +            1.5, -1.6, 3141.6, -3141.6/
        real dcos(8)
        data dcos/0.01, -0.01, 0.7854, -0.7854,
     +            1.5, -1.6, 3141.6, -3141.6/
        real datan(8)
        data datan/0.0001, -0.0001, 0.5, -0.5,
     +             2, -2, 999.9, -999.9/
        real dasin(8)
        data dasin/0.0001, -0.0001, 0.5, -0.5,
     +             0.99, -0.99, 0.9999, -0.9999/
        real dlog(8)
        data dlog/0.0001, 0.5, 0.9999, 1.0001,
     +            2.0, 4.0, 8.0, 99999.0/
        real dexp(5)
        data dexp/-6.0, -1.0, 0.0001, 1.0, 6.0/
        real dfloor(8)
        data dfloor/-98.7777, 98.7777, -5.4444, 5.4444,
     +              -0.9999, 0.9999, -0.1111, 0.1111/
        name = ' sqrt'
        do 10 i=1, 5
            x = dsqrt(i)
            a = e50(x)
 10         print 100, name, x, a
 100    format(' ', A5, '(', F10.4, ') = ', F10.6)
        name = '  sin'
        do 20 i=1, 8
            x = dsin(i)
            a = e50a1(x)
 20         print 100, name, x, a
        name = '  sin'
        do 25 i=1, 8
            x = dsin(i)
            a = e51(x)
 25         print 100, name, x, a
        name = '  cos'
        do 30 i=1, 8
            x = dcos(i)
            a = e50a2(x)
 30         print 100, name, x, a
        name = '  cos'
        do 35 i=1, 8
            x = dcos(i)
            a = e52(x)
 35         print 100, name, x, a
        name = ' atan'
        do 40 i=1, 8
            x = datan(i)
            a = e50a3(x)
 40         print 100, name, x, a
        name = ' atan'
        do 45 i=1, 8
            x = datan(i)
            a = e53(x)
 45         print 100, name, x, a
        name = ' asin'
        do 50 i=1, 8
            x = dasin(i)
            a = e50a4(x)
 50         print 100, name, x, a
        name = ' asin'
        do 55 i=1, 8
            x = dasin(i)
            a = e54(x)
 55         print 100, name, x, a
        name = '  log'
        do 60 i=1, 8
            x = dlog(i)
            a = e50a5(x)
 60         print 100, name, x, a
        name = '  log'
        do 65 i=1, 8
            x = dlog(i)
            a = e55(x)
 65         print 100, name, x, a
        name = '  exp'
        do 70 i=1, 5
            x = dexp(i)
            a = e50a6(x)
 70         print 100, name, x, a
        name = '  exp'
        do 75 i=1, 5
            x = dexp(i)
            a = e56(x)
 75         print 100, name, x, a
        name = 'floor'
        do 80 i=1, 8
            x = dfloor(i)
            a = e50a7(x)
 80         print 100, name, x, a
        name = 'floor'
        do 85 i=1, 8
            x = dfloor(i)
            a = e57(x)
 85         print 100, name, x, a
        end
*assem
 e50    :   ,name,      . sqrt(x)
            ,ati , 11
         11 ,xta ,
            ,*50 , 0
         13 ,uj  ,
            ,end ,
 e50a1  :   ,name,      . sin(x)
            ,ati , 11
         11 ,xta ,
            ,*50 , 1
         13 ,uj  ,
            ,end ,
 e50a2  :   ,name,      . cos(x)
            ,ati , 11
         11 ,xta ,
            ,*50 , 2
         13 ,uj  ,
            ,end ,
 e50a3  :   ,name,      . atan(x)
            ,ati , 11
         11 ,xta ,
            ,*50 , 3
         13 ,uj  ,
            ,end ,
 e50a4  :   ,name,      . asin(x)
            ,ati , 11
         11 ,xta ,
            ,*50 , 4
         13 ,uj  ,
            ,end ,
 e50a5  :   ,name,      . log(x)
            ,ati , 11
         11 ,xta ,
            ,*50 , 5
         13 ,uj  ,
            ,end ,
 e50a6  :   ,name,      . exp(x)
            ,ati , 11
         11 ,xta ,
            ,*50 , 6
         13 ,uj  ,
            ,end ,
 e50a7  :   ,name,      . floor(x)
            ,ati , 11
         11 ,xta ,
            ,*50 , 7
         13 ,uj  ,
            ,end ,
 e51    :   ,name,      . sin(x)
            ,ati , 11
         11 ,xta ,
            ,*51 ,
         13 ,uj  ,
            ,end ,
 e52    :   ,name,      . cos(x)
            ,ati , 11
         11 ,xta ,
            ,*52 ,
         13 ,uj  ,
            ,end ,
 e53    :   ,name,      . atan(x)
            ,ati , 11
         11 ,xta ,
            ,*53 ,
         13 ,uj  ,
            ,end ,
 e54    :   ,name,      . asin(x)
            ,ati , 11
         11 ,xta ,
            ,*54 ,
         13 ,uj  ,
            ,end ,
 e55    :   ,name,      . log(x)
            ,ati , 11
         11 ,xta ,
            ,*55 ,
         13 ,uj  ,
            ,end ,
 e56    :   ,name,      . exp(x)
            ,ati , 11
         11 ,xta ,
            ,*56 ,
         13 ,uj  ,
            ,end ,
 e57    :   ,name,      . floor(x)
            ,ati , 11
         11 ,xta ,
            ,*57 ,
         13 ,uj  ,
            ,end ,
*no load list
*execute
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_elem_func.txt");
    check_output(output, expect);
}

TEST_F(dubna_session, e50_print_real)
{
    auto output = run_job_and_capture_output(R"(*name e50 17
*no list
*ftn
        subroutine format(value, n, m)
        integer buf(5), overfl
        buf(1) = '      '
        buf(2) = '      '
        buf(3) = '      '
        buf(4) = '      '
        buf(5) = '      '
        call e50a17(buf(1), value, n, m, overfl)
        call printa(buf(1), buf(5), 1)
        if (overfl .ne. 0) print 10
 10     format(' overflow')
        return
        end
        program main
        pi = 4 * atan(1.0)
        call format(pi, 4+18, 15)
        call format(pi, 4+13, 10)
        call format(pi, 4+5, 2)
        call format(pi, 4+4, 1)
        call format(pi, 4+4, 0)
        call format(pi, 4+3, 1)
        call format(pi, 4+2, 1)
        call format(pi, 4+1, 0)
        call format(-pi, 4+18, 15)
        call format(-pi, 4+13, 10)
        call format(-pi, 4+5, 2)
        call format(-pi, 4+4, 1)
        call format(-pi, 4+4, 0)
        call format(-pi, 4+3, 1)
        call format(-pi, 4+2, 1)
        call format(-pi, 4+1, 0)
        call format(1.23456789123456789e3, 4+18, 15)
        call format(1.23456789123456789e3, 4+13, 10)
        call format(1.23456789123456789e3, 4+5, 2)
        call format(1.23456789123456789e3, 4+4, 1)
        call format(1.23456789123456789e3, 4+4, 0)
        call format(1.23456789123456789e3, 4+3, 1)
        call format(1.23456789123456789e3, 4+2, 1)
        call format(1.23456789123456789e3, 4+1, 0)
        call format(-1.23456789123456789e3, 4+18, 15)
        call format(-1.23456789123456789e3, 4+13, 10)
        call format(-1.23456789123456789e3, 4+5, 2)
        call format(-1.23456789123456789e3, 4+4, 1)
        call format(-1.23456789123456789e3, 4+4, 0)
        call format(-1.23456789123456789e3, 4+3, 1)
        call format(-1.23456789123456789e3, 4+2, 1)
        call format(-1.23456789123456789e3, 4+1, 0)
        call format(1.23456789123456789e10, 4+18, 15)
        call format(1.23456789123456789e10, 4+13, 10)
        call format(1.23456789123456789e10, 4+5, 2)
        call format(1.23456789123456789e10, 4+4, 1)
        call format(1.23456789123456789e10, 4+4, 0)
        call format(1.23456789123456789e10, 4+3, 1)
        call format(1.23456789123456789e10, 4+2, 1)
        call format(1.23456789123456789e10, 4+1, 0)
        call format(-1.23456789123456789e10, 4+18, 15)
        call format(-1.23456789123456789e10, 4+13, 10)
        call format(-1.23456789123456789e10, 4+5, 2)
        call format(-1.23456789123456789e10, 4+4, 1)
        call format(-1.23456789123456789e10, 4+4, 0)
        call format(-1.23456789123456789e10, 4+3, 1)
        call format(-1.23456789123456789e10, 4+2, 1)
        call format(-1.23456789123456789e10, 4+1, 0)
        call format(1.23456789123456789e-10, 6+18, 5)
        call format(1.23456789123456789e-10, 6+13, 1)
        call format(1.23456789123456789e-10, 4+5, 2)
        call format(1.23456789123456789e-10, 4+4, 1)
        call format(1.23456789123456789e-10, 4+4, 0)
        call format(1.23456789123456789e-10, 4+3, 1)
        call format(1.23456789123456789e-10, 4+2, 1)
        call format(1.23456789123456789e-10, 4+1, 0)
        call format(-1.23456789123456789e-10, 6+18, 5)
        call format(-1.23456789123456789e-10, 6+13, 1)
        call format(-1.23456789123456789e-10, 4+5, 2)
        call format(-1.23456789123456789e-10, 4+4, 1)
        call format(-1.23456789123456789e-10, 4+4, 0)
        call format(-1.23456789123456789e-10, 4+3, 1)
        call format(-1.23456789123456789e-10, 4+2, 1)
        call format(-1.23456789123456789e-10, 4+1, 0)
        end
*assem
 e50a17 :   ,name,
            ,sti , 12
            ,sti , 11
            ,sti , 10
            ,sti , 9
            ,sti , 8
            ,ita , 8
            ,its , 9
            ,asn , 64-24
         15 ,aox ,
         10 ,xts ,
            ,asn , 64-39
         15 ,aox ,
         11 ,xts ,
            ,asn , 64-15
         15 ,aox ,
            ,*50 , 17в
            ,its , 14
            ,aox ,=:64
         12 ,stx ,
         13 ,uj  ,
            ,end ,
*no load list
*execute
*end file
)");
    auto expect = file_contents(TEST_DIR "/expect_e50_print_real.txt");
    check_output(output, expect);
}
