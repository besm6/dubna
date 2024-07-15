//
// Tests for CERN libraries.
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

#include "fixture_machine.h"

//
// These tests are enabled by "make test-all" in the top level Makefile.
// Please note that this file adds more than 400 tests to the list.
//
TEST_F(dubna_machine, cernlib_a200)
{
    test_cernlib(1, "a200");
}
TEST_F(dubna_machine, cernlib_a400)
{
    test_cernlib(1, "a400");
}
TEST_F(dubna_machine, cernlib_a401)
{
    test_cernlib(1, "a401");
}
TEST_F(dubna_machine, cernlib_a402)
{
    test_cernlib(1, "a402");
}
TEST_F(dubna_machine, cernlib_a403)
{
    test_cernlib(1, "a403");
}
TEST_F(dubna_machine, cernlib_a404)
{
    test_cernlib(1, "a404");
}
TEST_F(dubna_machine, cernlib_a500)
{
    test_cernlib(1, "a500");
}
TEST_F(dubna_machine, cernlib_b101)
{
    test_cernlib(1, "b101");
}
TEST_F(dubna_machine, cernlib_b102)
{
    test_cernlib(1, "b102");
}
TEST_F(dubna_machine, cernlib_b200)
{
    test_cernlib(1, "b200");
}
TEST_F(dubna_machine, cernlib_b400)
{
    test_cernlib(1, "b400");
}
TEST_F(dubna_machine, cernlib_c100)
{
    test_cernlib(1, "c100");
}
TEST_F(dubna_machine, cernlib_c101)
{
    test_cernlib(1, "c101");
}
TEST_F(dubna_machine, cernlib_c110)
{
    test_cernlib(1, "c110");
}
TEST_F(dubna_machine, cernlib_c201)
{
    test_cernlib(1, "c201");
}
TEST_F(dubna_machine, cernlib_c202)
{
    test_cernlib(1, "c202");
}
TEST_F(dubna_machine, cernlib_c203)
{
    test_cernlib(1, "c203");
}
TEST_F(dubna_machine, cernlib_c204)
{
    test_cernlib(1, "c204");
}
TEST_F(dubna_machine, cernlib_c205)
{
    test_cernlib(1, "c205");
}
TEST_F(dubna_machine, cernlib_c206a)
{
    test_cernlib(1, "c206a");
}
TEST_F(dubna_machine, cernlib_c206b)
{
    test_cernlib(1, "c206b");
}
TEST_F(dubna_machine, cernlib_c206c)
{
    test_cernlib(1, "c206c");
}
TEST_F(dubna_machine, cernlib_c230)
{
    test_cernlib(1, "c230");
}
TEST_F(dubna_machine, cernlib_c231)
{
    test_cernlib(1, "c231");
}
TEST_F(dubna_machine, cernlib_c300)
{
    test_cernlib(1, "c300");
}
TEST_F(dubna_machine, cernlib_c301)
{
    test_cernlib(1, "c301");
}
TEST_F(dubna_machine, cernlib_c302)
{
    test_cernlib(1, "c302");
}
TEST_F(dubna_machine, cernlib_c302b)
{
    test_cernlib(1, "c302b");
}
TEST_F(dubna_machine, cernlib_c303a)
{
    test_cernlib(1, "c303a");
}
TEST_F(dubna_machine, cernlib_c303b)
{
    test_cernlib(1, "c303b");
}
TEST_F(dubna_machine, cernlib_c304)
{
    test_cernlib(1, "c304");
}
TEST_F(dubna_machine, cernlib_c305)
{
    test_cernlib(1, "c305");
}
TEST_F(dubna_machine, cernlib_c306)
{
    test_cernlib(1, "c306");
}
TEST_F(dubna_machine, cernlib_c307)
{
    test_cernlib(1, "c307");
}
TEST_F(dubna_machine, cernlib_c308)
{
    test_cernlib(1, "c308");
}
TEST_F(dubna_machine, cernlib_c309)
{
    test_cernlib(1, "c309");
}
TEST_F(dubna_machine, cernlib_c310)
{
    test_cernlib(1, "c310");
}
TEST_F(dubna_machine, cernlib_c311)
{
    test_cernlib(1, "c311");
}
TEST_F(dubna_machine, cernlib_c312)
{
    test_cernlib(1, "c312");
}
TEST_F(dubna_machine, cernlib_c312d)
{
    test_cernlib(1, "c312d");
}
TEST_F(dubna_machine, cernlib_c313)
{
    test_cernlib(1, "c313");
}
TEST_F(dubna_machine, cernlib_c313d)
{
    test_cernlib(1, "c313d");
}
TEST_F(dubna_machine, cernlib_c314)
{
    test_cernlib(1, "c314");
}
TEST_F(dubna_machine, cernlib_c315)
{
    test_cernlib(1, "c315");
}
TEST_F(dubna_machine, cernlib_c316a)
{
    test_cernlib(1, "c316a");
}
TEST_F(dubna_machine, cernlib_c316b)
{
    test_cernlib(1, "c316b");
}
TEST_F(dubna_machine, cernlib_c317)
{
    test_cernlib(1, "c317");
}
TEST_F(dubna_machine, cernlib_c318)
{
    test_cernlib(1, "c318");
}
TEST_F(dubna_machine, cernlib_c318a)
{
    test_cernlib(1, "c318a");
}
TEST_F(dubna_machine, cernlib_c319)
{
    test_cernlib(1, "c319");
}
TEST_F(dubna_machine, cernlib_c320)
{
    test_cernlib(1, "c320");
}
TEST_F(dubna_machine, cernlib_c322)
{
    test_cernlib(1, "c322");
}
TEST_F(dubna_machine, cernlib_c323)
{
    test_cernlib(1, "c323");
}
TEST_F(dubna_machine, cernlib_c324)
{
    test_cernlib(1, "c324");
}
TEST_F(dubna_machine, cernlib_c325a)
{
    test_cernlib(1, "c325a");
}
TEST_F(dubna_machine, cernlib_c325b)
{
    test_cernlib(1, "c325b");
}
TEST_F(dubna_machine, cernlib_c326)
{
    test_cernlib(1, "c326");
}
TEST_F(dubna_machine, cernlib_c327)
{
    test_cernlib(1, "c327");
}
TEST_F(dubna_machine, cernlib_c329a)
{
    test_cernlib(1, "c329a");
}
TEST_F(dubna_machine, cernlib_c329b)
{
    test_cernlib(1, "c329b");
}
TEST_F(dubna_machine, cernlib_c330a)
{
    test_cernlib(1, "c330a");
}
TEST_F(dubna_machine, cernlib_c330b)
{
    test_cernlib(1, "c330b");
}
TEST_F(dubna_machine, cernlib_c332a)
{
    test_cernlib(1, "c332a");
}
TEST_F(dubna_machine, cernlib_c332b)
{
    test_cernlib(1, "c332b");
}
TEST_F(dubna_machine, cernlib_c333)
{
    test_cernlib(1, "c333");
}
TEST_F(dubna_machine, cernlib_c335)
{
    test_cernlib(1, "c335");
}
TEST_F(dubna_machine, cernlib_c336)
{
    test_cernlib(1, "c336");
}
TEST_F(dubna_machine, cernlib_c337)
{
    test_cernlib(1, "c337");
}
TEST_F(dubna_machine, cernlib_c338)
{
    test_cernlib(1, "c338");
}
TEST_F(dubna_machine, cernlib_c339)
{
    test_cernlib(1, "c339");
}
TEST_F(dubna_machine, cernlib_c340)
{
    test_cernlib(1, "c340");
}
TEST_F(dubna_machine, cernlib_c341)
{
    test_cernlib(1, "c341");
}
TEST_F(dubna_machine, cernlib_c342)
{
    test_cernlib(1, "c342");
}
TEST_F(dubna_machine, cernlib_c349)
{
    test_cernlib(1, "c349");
}
TEST_F(dubna_machine, cernlib_c350a)
{
    test_cernlib(1, "c350a");
}
TEST_F(dubna_machine, cernlib_c350b)
{
    test_cernlib(1, "c350b");
}
TEST_F(dubna_machine, cernlib_c351)
{
    test_cernlib(1, "c351");
}
TEST_F(dubna_machine, cernlib_c360)
{
    test_cernlib(1, "c360");
}
TEST_F(dubna_machine, cernlib_c368)
{
    test_cernlib(1, "c368");
}
TEST_F(dubna_machine, cernlib_c400)
{
    test_cernlib(1, "c400");
}
TEST_F(dubna_machine, cernlib_c401)
{
    test_cernlib(1, "c401");
}
TEST_F(dubna_machine, cernlib_d100)
{
    test_cernlib(1, "d100");
}
TEST_F(dubna_machine, cernlib_d101)
{
    test_cernlib(1, "d101");
}
TEST_F(dubna_machine, cernlib_d102)
{
    test_cernlib(1, "d102");
}
TEST_F(dubna_machine, cernlib_d103)
{
    test_cernlib(1, "d103");
}
TEST_F(dubna_machine, cernlib_d104)
{
    test_cernlib(1, "d104");
}
TEST_F(dubna_machine, cernlib_d105)
{
    test_cernlib(1, "d105");
}
TEST_F(dubna_machine, cernlib_d106)
{
    test_cernlib(1, "d106");
}
TEST_F(dubna_machine, cernlib_d108)
{
    test_cernlib(1, "d108");
}
TEST_F(dubna_machine, cernlib_d109)
{
    test_cernlib(1, "d109");
}
TEST_F(dubna_machine, cernlib_d110)
{
    test_cernlib(1, "d110");
}
TEST_F(dubna_machine, cernlib_d111)
{
    test_cernlib(1, "d111");
}
TEST_F(dubna_machine, cernlib_d113)
{
    test_cernlib(1, "d113");
}
TEST_F(dubna_machine, cernlib_d114)
{
    test_cernlib(1, "d114");
}
TEST_F(dubna_machine, cernlib_d1141)
{
    test_cernlib(1, "d1141");
}
TEST_F(dubna_machine, cernlib_d115)
{
    test_cernlib(1, "d115");
}
TEST_F(dubna_machine, cernlib_d120)
{
    test_cernlib(1, "d120");
}
TEST_F(dubna_machine, cernlib_d121)
{
    test_cernlib(1, "d121");
}
TEST_F(dubna_machine, cernlib_d122)
{
    test_cernlib(1, "d122");
}
TEST_F(dubna_machine, cernlib_d125)
{
    test_cernlib(1, "d125");
}
TEST_F(dubna_machine, cernlib_d126)
{
    test_cernlib(1, "d126");
}
TEST_F(dubna_machine, cernlib_d129)
{
    test_cernlib(1, "d129");
}
TEST_F(dubna_machine, cernlib_d130)
{
    test_cernlib(1, "d130");
}
TEST_F(dubna_machine, cernlib_d130a)
{
    test_cernlib(1, "d130a");
}
TEST_F(dubna_machine, cernlib_d131)
{
    test_cernlib(1, "d131");
}
TEST_F(dubna_machine, cernlib_d131a)
{
    test_cernlib(1, "d131a");
}
//TEST_F(dubna_machine, cernlib_d150)
//{
//    test_cernlib(1, "d150"); // This test fails when loading: OTCYTCTBYET  DFUN
//}
TEST_F(dubna_machine, cernlib_d151)
{
    test_cernlib(1, "d151");
}
TEST_F(dubna_machine, cernlib_d201)
{
    test_cernlib(1, "d201");
}
TEST_F(dubna_machine, cernlib_d202)
{
    test_cernlib(1, "d202");
}
TEST_F(dubna_machine, cernlib_d203)
{
    test_cernlib(1, "d203");
}
TEST_F(dubna_machine, cernlib_d204)
{
    test_cernlib(1, "d204");
}
TEST_F(dubna_machine, cernlib_d205)
{
    test_cernlib(1, "d205");
}
TEST_F(dubna_machine, cernlib_d206)
{
    test_cernlib(1, "d206");
}
TEST_F(dubna_machine, cernlib_d207)
{
    test_cernlib(1, "d207");
}
TEST_F(dubna_machine, cernlib_d208)
{
    test_cernlib(1, "d208");
}
TEST_F(dubna_machine, cernlib_d209)
{
    test_cernlib(1, "d209");
}
TEST_F(dubna_machine, cernlib_d221)
{
    test_cernlib(1, "d221");
}
TEST_F(dubna_machine, cernlib_d300)
{
    test_cernlib(1, "d300");
}
TEST_F(dubna_machine, cernlib_d302)
{
    test_cernlib(1, "d302");
}
TEST_F(dubna_machine, cernlib_d400)
{
    test_cernlib(1, "d400");
}
TEST_F(dubna_machine, cernlib_d500)
{
    test_cernlib(1, "d500");
}
TEST_F(dubna_machine, cernlib_d504)
{
    test_cernlib(1, "d504");
}
TEST_F(dubna_machine, cernlib_d505)
{
    test_cernlib(1, "d505");
}
TEST_F(dubna_machine, cernlib_d506)
{
    test_cernlib(1, "d506");
}
TEST_F(dubna_machine, cernlib_d507)
{
    test_cernlib(1, "d507");
}
TEST_F(dubna_machine, cernlib_d508)
{
    test_cernlib(1, "d508");
}
TEST_F(dubna_machine, cernlib_d509)
{
    test_cernlib(1, "d509");
}
TEST_F(dubna_machine, cernlib_d510)
{
    test_cernlib(1, "d510");
}
TEST_F(dubna_machine, cernlib_d515)
{
    test_cernlib(1, "d515");
}
TEST_F(dubna_machine, cernlib_d522)
{
    test_cernlib(1, "d522");
}
TEST_F(dubna_machine, cernlib_d540)
{
    test_cernlib(1, "d540");
}
TEST_F(dubna_machine, cernlib_d600)
{
    test_cernlib(1, "d600");
}
TEST_F(dubna_machine, cernlib_d610)
{
    test_cernlib(1, "d610");
}
TEST_F(dubna_machine, cernlib_d612)
{
    test_cernlib(1, "d612");
}
TEST_F(dubna_machine, cernlib_d612a)
{
    test_cernlib(1, "d612a");
}
TEST_F(dubna_machine, cernlib_d700)
{
    test_cernlib(1, "d700");
}
TEST_F(dubna_machine, cernlib_d701)
{
    test_cernlib(1, "d701");
}
TEST_F(dubna_machine, cernlib_d702)
{
    test_cernlib(1, "d702");
}
TEST_F(dubna_machine, cernlib_d703)
{
    test_cernlib(1, "d703");
}
TEST_F(dubna_machine, cernlib_d704)
{
    test_cernlib(1, "d704");
}
TEST_F(dubna_machine, cernlib_d704b)
{
    test_cernlib(1, "d704b");
}
TEST_F(dubna_machine, cernlib_d800)
{
    test_cernlib(1, "d800");
}
TEST_F(dubna_machine, cernlib_e100)
{
    test_cernlib(1, "e100");
}
TEST_F(dubna_machine, cernlib_e101)
{
    test_cernlib(1, "e101");
}
TEST_F(dubna_machine, cernlib_e102)
{
    test_cernlib(1, "e102");
}
TEST_F(dubna_machine, cernlib_e103)
{
    test_cernlib(1, "e103");
}
TEST_F(dubna_machine, cernlib_e104)
{
    test_cernlib(1, "e104");
}
TEST_F(dubna_machine, cernlib_e104c)
{
    test_cernlib(1, "e104c");
}
TEST_F(dubna_machine, cernlib_e104t)
{
    test_cernlib(1, "e104t");
}
TEST_F(dubna_machine, cernlib_e105)
{
    test_cernlib(1, "e105");
}
TEST_F(dubna_machine, cernlib_e106)
{
    test_cernlib(1, "e106");
}
TEST_F(dubna_machine, cernlib_e110)
{
    test_cernlib(1, "e110");
}
TEST_F(dubna_machine, cernlib_e110a)
{
    test_cernlib(1, "e110a");
}
TEST_F(dubna_machine, cernlib_e111)
{
    test_cernlib(1, "e111");
}
TEST_F(dubna_machine, cernlib_e112)
{
    test_cernlib(1, "e112");
}
TEST_F(dubna_machine, cernlib_e113)
{
    test_cernlib(1, "e113");
}
TEST_F(dubna_machine, cernlib_e200)
{
    test_cernlib(1, "e200");
}
TEST_F(dubna_machine, cernlib_e201)
{
    test_cernlib(1, "e201");
}
TEST_F(dubna_machine, cernlib_e202)
{
    test_cernlib(1, "e202");
}
TEST_F(dubna_machine, cernlib_e204)
{
    test_cernlib(1, "e204");
}
TEST_F(dubna_machine, cernlib_e205)
{
    test_cernlib(1, "e205");
}
TEST_F(dubna_machine, cernlib_e206)
{
    test_cernlib(1, "e206");
}
TEST_F(dubna_machine, cernlib_e207)
{
    test_cernlib(1, "e207");
}
TEST_F(dubna_machine, cernlib_e208)
{
    test_cernlib(1, "e208");
}
TEST_F(dubna_machine, cernlib_e209)
{
    test_cernlib(1, "e209");
}
TEST_F(dubna_machine, cernlib_e220)
{
    test_cernlib(1, "e220");
}
TEST_F(dubna_machine, cernlib_e221)
{
    test_cernlib(1, "e221");
}
TEST_F(dubna_machine, cernlib_e230)
{
    test_cernlib(1, "e230");
}
TEST_F(dubna_machine, cernlib_e230a)
{
    test_cernlib(1, "e230a");
}
TEST_F(dubna_machine, cernlib_e240)
{
    test_cernlib(1, "e240");
}
TEST_F(dubna_machine, cernlib_e250)
{
    test_cernlib(1, "e250");
}
TEST_F(dubna_machine, cernlib_e255)
{
    test_cernlib(1, "e255");
}
TEST_F(dubna_machine, cernlib_e400)
{
    test_cernlib(1, "e400");
}
TEST_F(dubna_machine, cernlib_e401)
{
    test_cernlib(1, "e401");
}
TEST_F(dubna_machine, cernlib_e402)
{
    test_cernlib(1, "e402");
}
TEST_F(dubna_machine, cernlib_e403)
{
    test_cernlib(1, "e403");
}
TEST_F(dubna_machine, cernlib_e404)
{
    test_cernlib(1, "e404");
}
TEST_F(dubna_machine, cernlib_e405)
{
    test_cernlib(1, "e405");
}
TEST_F(dubna_machine, cernlib_e406)
{
    test_cernlib(1, "e406");
}
TEST_F(dubna_machine, cernlib_e410)
{
    test_cernlib(1, "e410");
}
//TEST_F(dubna_machine, cernlib_e410a)
//{
//    test_cernlib(1, "e410a"); // This test fails when loading: OTCYTCTBYET  DTOC CTOD
//}
TEST_F(dubna_machine, cernlib_f002a)
{
    test_cernlib(2, "f002a");
}
TEST_F(dubna_machine, cernlib_f002b)
{
    test_cernlib(2, "f002b");
}
TEST_F(dubna_machine, cernlib_f002c)
{
    test_cernlib(2, "f002c");
}
TEST_F(dubna_machine, cernlib_f003a)
{
    test_cernlib(2, "f003a");
}
TEST_F(dubna_machine, cernlib_f003b)
{
    test_cernlib(2, "f003b");
}
TEST_F(dubna_machine, cernlib_f003c)
{
    test_cernlib(2, "f003c");
}
TEST_F(dubna_machine, cernlib_f004a)
{
    test_cernlib(2, "f004a");
}
TEST_F(dubna_machine, cernlib_f004b)
{
    test_cernlib(2, "f004b");
}
TEST_F(dubna_machine, cernlib_f004c)
{
    test_cernlib(2, "f004c");
}
TEST_F(dubna_machine, cernlib_f010a)
{
    test_cernlib(2, "f010a");
}
TEST_F(dubna_machine, cernlib_f010b)
{
    test_cernlib(2, "f010b");
}
TEST_F(dubna_machine, cernlib_f010c)
{
    test_cernlib(2, "f010c");
}
TEST_F(dubna_machine, cernlib_f011a)
{
    test_cernlib(2, "f011a");
}
TEST_F(dubna_machine, cernlib_f011b)
{
    test_cernlib(2, "f011b");
}
TEST_F(dubna_machine, cernlib_f011c)
{
    test_cernlib(2, "f011c");
}
TEST_F(dubna_machine, cernlib_f012a)
{
    test_cernlib(2, "f012a");
}
TEST_F(dubna_machine, cernlib_f012b)
{
    test_cernlib(2, "f012b");
}
TEST_F(dubna_machine, cernlib_f100)
{
    test_cernlib(2, "f100");
}
TEST_F(dubna_machine, cernlib_f101)
{
    test_cernlib(2, "f101");
}
TEST_F(dubna_machine, cernlib_f102)
{
    test_cernlib(2, "f102");
}
TEST_F(dubna_machine, cernlib_f103)
{
    test_cernlib(2, "f103");
}
TEST_F(dubna_machine, cernlib_f104)
{
    test_cernlib(2, "f104");
}
TEST_F(dubna_machine, cernlib_f105)
{
    test_cernlib(2, "f105");
}
TEST_F(dubna_machine, cernlib_f106a)
{
    test_cernlib(2, "f106a");
}
TEST_F(dubna_machine, cernlib_f106b)
{
    test_cernlib(2, "f106b");
}
TEST_F(dubna_machine, cernlib_f107)
{
    test_cernlib(2, "f107");
}
TEST_F(dubna_machine, cernlib_f108)
{
    test_cernlib(2, "f108");
}
TEST_F(dubna_machine, cernlib_f109)
{
    test_cernlib(2, "f109");
}
TEST_F(dubna_machine, cernlib_f110)
{
    test_cernlib(2, "f110");
}
TEST_F(dubna_machine, cernlib_f111)
{
    test_cernlib(2, "f111");
}
TEST_F(dubna_machine, cernlib_f112)
{
    test_cernlib(2, "f112");
}
TEST_F(dubna_machine, cernlib_f112a)
{
    test_cernlib(2, "f112a");
}
TEST_F(dubna_machine, cernlib_f115)
{
    test_cernlib(2, "f115");
}
TEST_F(dubna_machine, cernlib_f116)
{
    test_cernlib(2, "f116");
}
TEST_F(dubna_machine, cernlib_f117)
{
    test_cernlib(2, "f117");
}
TEST_F(dubna_machine, cernlib_f118)
{
    test_cernlib(2, "f118");
}
TEST_F(dubna_machine, cernlib_f119)
{
    test_cernlib(2, "f119");
}
TEST_F(dubna_machine, cernlib_f120)
{
    test_cernlib(2, "f120");
}
TEST_F(dubna_machine, cernlib_f121)
{
    test_cernlib(2, "f121");
}
TEST_F(dubna_machine, cernlib_f122)
{
    test_cernlib(2, "f122");
}
TEST_F(dubna_machine, cernlib_f123)
{
    test_cernlib(2, "f123");
}
TEST_F(dubna_machine, cernlib_f124)
{
    test_cernlib(2, "f124");
}
TEST_F(dubna_machine, cernlib_f133)
{
    test_cernlib(2, "f133");
}
TEST_F(dubna_machine, cernlib_f136)
{
    test_cernlib(2, "f136");
}
TEST_F(dubna_machine, cernlib_f140)
{
    test_cernlib(2, "f140");
}
TEST_F(dubna_machine, cernlib_f141)
{
    test_cernlib(2, "f141");
}
TEST_F(dubna_machine, cernlib_f142)
{
    test_cernlib(2, "f142");
}
TEST_F(dubna_machine, cernlib_f143)
{
    test_cernlib(2, "f143");
}
TEST_F(dubna_machine, cernlib_f144)
{
    test_cernlib(2, "f144");
}
TEST_F(dubna_machine, cernlib_f145)
{
    test_cernlib(2, "f145");
}
TEST_F(dubna_machine, cernlib_f146)
{
    test_cernlib(2, "f146");
}
TEST_F(dubna_machine, cernlib_f147)
{
    test_cernlib(2, "f147");
}
TEST_F(dubna_machine, cernlib_f150)
{
    test_cernlib(2, "f150");
}
TEST_F(dubna_machine, cernlib_f200)
{
    test_cernlib(2, "f200");
}
TEST_F(dubna_machine, cernlib_f201)
{
    test_cernlib(2, "f201");
}
TEST_F(dubna_machine, cernlib_f202)
{
    test_cernlib(2, "f202");
}
TEST_F(dubna_machine, cernlib_f203)
{
    test_cernlib(2, "f203");
}
TEST_F(dubna_machine, cernlib_f220)
{
    test_cernlib(2, "f220");
}
TEST_F(dubna_machine, cernlib_f221)
{
    test_cernlib(2, "f221");
}
TEST_F(dubna_machine, cernlib_f222)
{
    test_cernlib(2, "f222");
}
TEST_F(dubna_machine, cernlib_f223)
{
    test_cernlib(2, "f223");
}
TEST_F(dubna_machine, cernlib_f224)
{
    test_cernlib(2, "f224");
}
TEST_F(dubna_machine, cernlib_f225)
{
    test_cernlib(2, "f225");
}
TEST_F(dubna_machine, cernlib_f230)
{
    test_cernlib(2, "f230");
}
TEST_F(dubna_machine, cernlib_f240)
{
    test_cernlib(2, "f240");
}
TEST_F(dubna_machine, cernlib_f301)
{
    test_cernlib(2, "f301");
}
TEST_F(dubna_machine, cernlib_f301a)
{
    test_cernlib(2, "f301a");
}
TEST_F(dubna_machine, cernlib_f301b)
{
    test_cernlib(2, "f301b");
}
TEST_F(dubna_machine, cernlib_f303)
{
    test_cernlib(2, "f303");
}
TEST_F(dubna_machine, cernlib_f311)
{
    test_cernlib(2, "f311");
}
TEST_F(dubna_machine, cernlib_f320)
{
    test_cernlib(2, "f320");
}
TEST_F(dubna_machine, cernlib_f401)
{
    test_cernlib(2, "f401");
}
TEST_F(dubna_machine, cernlib_f402)
{
    test_cernlib(2, "f402");
}
TEST_F(dubna_machine, cernlib_f403)
{
    test_cernlib(2, "f403");
}
TEST_F(dubna_machine, cernlib_f404)
{
    test_cernlib(2, "f404");
}
TEST_F(dubna_machine, cernlib_f405a)
{
    test_cernlib(2, "f405a");
}
TEST_F(dubna_machine, cernlib_f405b)
{
    test_cernlib(2, "f405b");
}
TEST_F(dubna_machine, cernlib_f406)
{
    test_cernlib(2, "f406");
}
TEST_F(dubna_machine, cernlib_f413)
{
    test_cernlib(2, "f413");
}
TEST_F(dubna_machine, cernlib_f420)
{
    test_cernlib(2, "f420");
}
TEST_F(dubna_machine, cernlib_f421)
{
    test_cernlib(2, "f421");
}
TEST_F(dubna_machine, cernlib_f422)
{
    test_cernlib(2, "f422");
}
TEST_F(dubna_machine, cernlib_f423)
{
    test_cernlib(2, "f423");
}
TEST_F(dubna_machine, cernlib_f424)
{
    test_cernlib(2, "f424");
}
TEST_F(dubna_machine, cernlib_f500)
{
    test_cernlib(2, "f500");
}
TEST_F(dubna_machine, cernlib_f600)
{
    test_cernlib(2, "f600");
}
TEST_F(dubna_machine, cernlib_g100)
{
    test_cernlib(2, "g100");
}
TEST_F(dubna_machine, cernlib_g101)
{
    test_cernlib(2, "g101");
}
TEST_F(dubna_machine, cernlib_g102)
{
    test_cernlib(2, "g102");
}
TEST_F(dubna_machine, cernlib_g104)
{
    test_cernlib(2, "g104");
}
TEST_F(dubna_machine, cernlib_g106)
{
    test_cernlib(2, "g106");
}
TEST_F(dubna_machine, cernlib_g110)
{
    test_cernlib(2, "g110");
}
TEST_F(dubna_machine, cernlib_g111)
{
    test_cernlib(2, "g111");
}
TEST_F(dubna_machine, cernlib_g200)
{
    test_cernlib(2, "g200");
}
TEST_F(dubna_machine, cernlib_g900)
{
    test_cernlib(2, "g900");
}
TEST_F(dubna_machine, cernlib_g901)
{
    test_cernlib(2, "g901");
}
TEST_F(dubna_machine, cernlib_h100)
{
    test_cernlib(2, "h100");
}
TEST_F(dubna_machine, cernlib_h300)
{
    test_cernlib(2, "h300");
}
TEST_F(dubna_machine, cernlib_h600)
{
    test_cernlib(2, "h600");
}
TEST_F(dubna_machine, cernlib_h601)
{
    test_cernlib(2, "h601");
}
TEST_F(dubna_machine, cernlib_h602)
{
    test_cernlib(2, "h602");
}
TEST_F(dubna_machine, cernlib_i302)
{
    test_cernlib(2, "i302");
}
TEST_F(dubna_machine, cernlib_i310)
{
    test_cernlib(2, "i310");
}
TEST_F(dubna_machine, cernlib_i312a)
{
    test_cernlib(2, "i312a");
}
TEST_F(dubna_machine, cernlib_i312b)
{
    test_cernlib(2, "i312b");
}
TEST_F(dubna_machine, cernlib_i901)
{
    test_cernlib(2, "i901");
}
TEST_F(dubna_machine, cernlib_j300)
{
    test_cernlib(2, "j300");
}
TEST_F(dubna_machine, cernlib_j500)
{
    test_cernlib(2, "j500");
}
TEST_F(dubna_machine, cernlib_j501)
{
    test_cernlib(2, "j501");
}
TEST_F(dubna_machine, cernlib_j502a)
{
    test_cernlib(2, "j502a");
}
TEST_F(dubna_machine, cernlib_j502b)
{
    test_cernlib(2, "j502b");
}
TEST_F(dubna_machine, cernlib_j502c)
{
    test_cernlib(2, "j502c");
}
TEST_F(dubna_machine, cernlib_j503)
{
    test_cernlib(2, "j503");
}
TEST_F(dubna_machine, cernlib_j504)
{
    test_cernlib(2, "j504");
}
TEST_F(dubna_machine, cernlib_j506)
{
    test_cernlib(2, "j506");
}
TEST_F(dubna_machine, cernlib_j508)
{
    test_cernlib(2, "j508");
}
TEST_F(dubna_machine, cernlib_j509)
{
    test_cernlib(2, "j509");
}
TEST_F(dubna_machine, cernlib_j509a)
{
    test_cernlib(2, "j509a");
}
TEST_F(dubna_machine, cernlib_j509b)
{
    test_cernlib(2, "j509b");
}
TEST_F(dubna_machine, cernlib_j511a)
{
    test_cernlib(2, "j511a");
}
TEST_F(dubna_machine, cernlib_j511b)
{
    test_cernlib(2, "j511b");
}
TEST_F(dubna_machine, cernlib_j520)
{
    test_cernlib(2, "j520");
}
TEST_F(dubna_machine, cernlib_j530)
{
    test_cernlib(2, "j530");
}
TEST_F(dubna_machine, cernlib_j531a)
{
    test_cernlib(2, "j531a");
}
TEST_F(dubna_machine, cernlib_j531b)
{
    test_cernlib(2, "j531b");
}
TEST_F(dubna_machine, cernlib_j540)
{
    test_cernlib(2, "j540");
}
TEST_F(dubna_machine, cernlib_j550)
{
    test_cernlib(2, "j550");
}
TEST_F(dubna_machine, cernlib_j551)
{
    test_cernlib(2, "j551");
}
TEST_F(dubna_machine, cernlib_k100)
{
    test_cernlib(2, "k100");
}
TEST_F(dubna_machine, cernlib_k101)
{
    test_cernlib(2, "k101");
}
TEST_F(dubna_machine, cernlib_k450)
{
    test_cernlib(2, "k450");
}
TEST_F(dubna_machine, cernlib_k451a)
{
    test_cernlib(2, "k451a");
}
TEST_F(dubna_machine, cernlib_k451b)
{
    test_cernlib(2, "k451b");
}
TEST_F(dubna_machine, cernlib_k452)
{
    test_cernlib(2, "k452");
}
TEST_F(dubna_machine, cernlib_m101)
{
    test_cernlib(2, "m101");
}
TEST_F(dubna_machine, cernlib_m103)
{
    test_cernlib(2, "m103");
}
TEST_F(dubna_machine, cernlib_m106)
{
    test_cernlib(2, "m106");
}
TEST_F(dubna_machine, cernlib_m108)
{
    test_cernlib(2, "m108");
}
TEST_F(dubna_machine, cernlib_m203)
{
    test_cernlib(2, "m203");
}
TEST_F(dubna_machine, cernlib_m204)
{
    test_cernlib(2, "m204");
}
TEST_F(dubna_machine, cernlib_m205)
{
    test_cernlib(2, "m205");
}
TEST_F(dubna_machine, cernlib_m210)
{
    test_cernlib(2, "m210");
}
TEST_F(dubna_machine, cernlib_m215)
{
    test_cernlib(2, "m215");
}
TEST_F(dubna_machine, cernlib_m216)
{
    test_cernlib(2, "m216");
}
TEST_F(dubna_machine, cernlib_m224)
{
    test_cernlib(2, "m224");
}
TEST_F(dubna_machine, cernlib_m250t)
{
    test_cernlib(2, "m250t");
}
//TEST_F(dubna_machine, cernlib_m251)
//{
//    test_cernlib(2, "m251"); // This test fails when loading: OTCYTCTBYET  MINSTA MINISO MINEND
//}
TEST_F(dubna_machine, cernlib_m254)
{
    test_cernlib(2, "m254");
}
TEST_F(dubna_machine, cernlib_m401)
{
    test_cernlib(2, "m401");
}
TEST_F(dubna_machine, cernlib_m402)
{
    test_cernlib(2, "m402");
}
TEST_F(dubna_machine, cernlib_m403)
{
    test_cernlib(2, "m403");
}
TEST_F(dubna_machine, cernlib_m404)
{
    test_cernlib(2, "m404");
}
TEST_F(dubna_machine, cernlib_m405)
{
    test_cernlib(2, "m405");
}
TEST_F(dubna_machine, cernlib_m407)
{
    test_cernlib(2, "m407");
}
TEST_F(dubna_machine, cernlib_m408)
{
    test_cernlib(2, "m408");
}
TEST_F(dubna_machine, cernlib_m409)
{
    test_cernlib(2, "m409");
}
TEST_F(dubna_machine, cernlib_m410)
{
    test_cernlib(2, "m410");
}
TEST_F(dubna_machine, cernlib_m412)
{
    test_cernlib(2, "m412");
}
TEST_F(dubna_machine, cernlib_m415)
{
    test_cernlib(2, "m415");
}
TEST_F(dubna_machine, cernlib_m416)
{
    test_cernlib(2, "m416");
}
TEST_F(dubna_machine, cernlib_m419)
{
    test_cernlib(2, "m419");
}
TEST_F(dubna_machine, cernlib_m421)
{
    test_cernlib(2, "m421");
}
TEST_F(dubna_machine, cernlib_m422)
{
    test_cernlib(2, "m422");
}
TEST_F(dubna_machine, cernlib_m423)
{
    test_cernlib(2, "m423");
}
TEST_F(dubna_machine, cernlib_m450)
{
    test_cernlib(2, "m450");
}
TEST_F(dubna_machine, cernlib_m501)
{
    test_cernlib(2, "m501");
}
TEST_F(dubna_machine, cernlib_m502)
{
    test_cernlib(2, "m502");
}
TEST_F(dubna_machine, cernlib_m506)
{
    test_cernlib(2, "m506");
}
TEST_F(dubna_machine, cernlib_n100)
{
    test_cernlib(2, "n100");
}
TEST_F(dubna_machine, cernlib_n200)
{
    test_cernlib(2, "n200");
}
TEST_F(dubna_machine, cernlib_q800)
{
    test_cernlib(2, "q800");
}
TEST_F(dubna_machine, cernlib_q820)
{
    test_cernlib(2, "q820");
}
TEST_F(dubna_machine, cernlib_q900a)
{
    test_cernlib(2, "q900a");
}
TEST_F(dubna_machine, cernlib_q900b)
{
    test_cernlib(2, "q900b");
}
TEST_F(dubna_machine, cernlib_relkin)
{
    test_cernlib(2, "relkin");
}
TEST_F(dubna_machine, cernlib_roses)
{
    test_cernlib(2, "roses");
}
TEST_F(dubna_machine, cernlib_si)
{
    test_cernlib(2, "si");
}
TEST_F(dubna_machine, cernlib_sincos)
{
    test_cernlib(2, "sincos");
}
TEST_F(dubna_machine, cernlib_t110a)
{
    test_cernlib(2, "t110a");
}
TEST_F(dubna_machine, cernlib_t110b)
{
    test_cernlib(2, "t110b");
}
TEST_F(dubna_machine, cernlib_t110c)
{
    test_cernlib(2, "t110c");
}
TEST_F(dubna_machine, cernlib_test)
{
    test_cernlib(2, "test");
}
TEST_F(dubna_machine, cernlib_test1)
{
    test_cernlib(1, "test1");
}
TEST_F(dubna_machine, cernlib_test2)
{
    test_cernlib(1, "test2");
}
TEST_F(dubna_machine, cernlib_test3)
{
    test_cernlib(1, "test3");
}
TEST_F(dubna_machine, cernlib_test4)
{
    test_cernlib(1, "test4");
}
TEST_F(dubna_machine, cernlib_test5)
{
    test_cernlib(1, "test5");
}
TEST_F(dubna_machine, cernlib_test6)
{
    test_cernlib(1, "test6");
}
TEST_F(dubna_machine, cernlib_u100)
{
    test_cernlib(2, "u100");
}
TEST_F(dubna_machine, cernlib_u101)
{
    test_cernlib(2, "u101");
}
TEST_F(dubna_machine, cernlib_u101a)
{
    test_cernlib(2, "u101a");
}
TEST_F(dubna_machine, cernlib_u102)
{
    test_cernlib(2, "u102");
}
TEST_F(dubna_machine, cernlib_u121)
{
    test_cernlib(2, "u121");
}
TEST_F(dubna_machine, cernlib_u500)
{
    test_cernlib(2, "u500");
}
TEST_F(dubna_machine, cernlib_v100)
{
    test_cernlib(2, "v100");
}
TEST_F(dubna_machine, cernlib_v102)
{
    test_cernlib(2, "v102");
}
TEST_F(dubna_machine, cernlib_v104)
{
    test_cernlib(2, "v104");
}
TEST_F(dubna_machine, cernlib_v106)
{
    test_cernlib(2, "v106");
}
TEST_F(dubna_machine, cernlib_v106a)
{
    test_cernlib(2, "v106a");
}
TEST_F(dubna_machine, cernlib_v110)
{
    test_cernlib(2, "v110");
}
TEST_F(dubna_machine, cernlib_v111)
{
    test_cernlib(2, "v111");
}
TEST_F(dubna_machine, cernlib_v112)
{
    test_cernlib(2, "v112");
}
TEST_F(dubna_machine, cernlib_v116)
{
    test_cernlib(2, "v116");
}
TEST_F(dubna_machine, cernlib_v130)
{
    test_cernlib(2, "v130");
}
TEST_F(dubna_machine, cernlib_v150)
{
    test_cernlib(2, "v150");
}
TEST_F(dubna_machine, cernlib_v151)
{
    test_cernlib(2, "v151");
}
TEST_F(dubna_machine, cernlib_v200)
{
    test_cernlib(2, "v200");
}
TEST_F(dubna_machine, cernlib_v201)
{
    test_cernlib(2, "v201");
}
TEST_F(dubna_machine, cernlib_v202)
{
    test_cernlib(2, "v202");
}
TEST_F(dubna_machine, cernlib_v300)
{
    test_cernlib(2, "v300");
}
TEST_F(dubna_machine, cernlib_v301)
{
    test_cernlib(2, "v301");
}
TEST_F(dubna_machine, cernlib_v302)
{
    test_cernlib(2, "v302");
}
TEST_F(dubna_machine, cernlib_v303)
{
    test_cernlib(2, "v303");
}
TEST_F(dubna_machine, cernlib_v304)
{
    test_cernlib(2, "v304");
}
TEST_F(dubna_machine, cernlib_v304a)
{
    test_cernlib(2, "v304a");
}
TEST_F(dubna_machine, cernlib_v305)
{
    test_cernlib(2, "v305");
}
TEST_F(dubna_machine, cernlib_v306)
{
    test_cernlib(2, "v306");
}
TEST_F(dubna_machine, cernlib_w100)
{
    test_cernlib(2, "w100");
}
TEST_F(dubna_machine, cernlib_w100a)
{
    test_cernlib(2, "w100a");
}
TEST_F(dubna_machine, cernlib_w101)
{
    test_cernlib(2, "w101");
}
TEST_F(dubna_machine, cernlib_w104)
{
    test_cernlib(2, "w104");
}
TEST_F(dubna_machine, cernlib_w105)
{
    test_cernlib(2, "w105");
}
TEST_F(dubna_machine, cernlib_w126)
{
    test_cernlib(2, "w126");
}
TEST_F(dubna_machine, cernlib_w127)
{
    test_cernlib(2, "w127");
}
TEST_F(dubna_machine, cernlib_w300)
{
    test_cernlib(2, "w300");
}
//TEST_F(dubna_machine, cernlib_w303)
//{
//    test_cernlib(2, "w303"); // This test loops forever
//}
TEST_F(dubna_machine, cernlib_w304)
{
    test_cernlib(2, "w304");
}
TEST_F(dubna_machine, cernlib_w307)
{
    test_cernlib(2, "w307");
}
TEST_F(dubna_machine, cernlib_w500)
{
    test_cernlib(2, "w500");
}
TEST_F(dubna_machine, cernlib_w501)
{
    test_cernlib(2, "w501");
}
TEST_F(dubna_machine, cernlib_w505)
{
    test_cernlib(2, "w505");
}
TEST_F(dubna_machine, cernlib_w520)
{
    test_cernlib(2, "w520");
}
TEST_F(dubna_machine, cernlib_w521)
{
    test_cernlib(2, "w521");
}
TEST_F(dubna_machine, cernlib_w601)
{
    test_cernlib(2, "w601");
}
TEST_F(dubna_machine, cernlib_w700)
{
    test_cernlib(2, "w700");
}
TEST_F(dubna_machine, cernlib_x202)
{
    test_cernlib(2, "x202");
}
TEST_F(dubna_machine, cernlib_x203)
{
    test_cernlib(2, "x203");
}
TEST_F(dubna_machine, cernlib_x401)
{
    test_cernlib(2, "x401");
}
TEST_F(dubna_machine, cernlib_x601)
{
    test_cernlib(2, "x601");
}
TEST_F(dubna_machine, cernlib_y201)
{
    test_cernlib(2, "y201");
}
TEST_F(dubna_machine, cernlib_y202)
{
    test_cernlib(2, "y202");
}
TEST_F(dubna_machine, cernlib_y203)
{
    test_cernlib(2, "y203");
}
TEST_F(dubna_machine, cernlib_z005)
{
    test_cernlib(2, "z005");
}
TEST_F(dubna_machine, cernlib_z054)
{
    test_cernlib(2, "z054");
}
