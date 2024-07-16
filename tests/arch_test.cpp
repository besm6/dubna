//
// Tests for routines from besm6_arch.cpp.
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
#include "util.h"

//
// Conversion from BESM-6 to IEEE floating point format.
//
TEST(arith, besm6_to_ieee)
{
    // clang-format off
    //                          2 - sign
    //                 exponent | ___mantissa___
    //                        /\|/              |
    EXPECT_EQ(besm6_to_ieee(0'0000'0000'0000'0000ull), 0.0);                    //  0.0
    EXPECT_EQ(besm6_to_ieee(0'0020'0000'0000'0000ull), -0x1p-64);               // -5.42101e-20
    EXPECT_EQ(besm6_to_ieee(0'0000'0000'0000'0001ull), 0x1p-104);               //  4.93038e-32
    EXPECT_EQ(besm6_to_ieee(0'0020'0000'0000'0001ull), -0x1.fffffffffep-65);    // -5.42101e-20
    EXPECT_EQ(besm6_to_ieee(0'0010'0000'0000'0000ull), 0x1p-65);                //  2.71051e-20
    EXPECT_EQ(besm6_to_ieee(0'0030'0000'0000'0000ull), -0x1p-65);               // -2.71051e-20
    EXPECT_EQ(besm6_to_ieee(0'0017'7777'7777'7777ull), 0x1.fffffffffep-65);     //  5.42101e-20
    EXPECT_EQ(besm6_to_ieee(0'0037'7777'7777'7777ull), -0x1p-104);              // -4.93038e-32
    EXPECT_EQ(besm6_to_ieee(0'0040'0000'0000'0000ull), 0.0);                    //  0.0
    EXPECT_EQ(besm6_to_ieee(0'0060'0000'0000'0000ull), -0x1p-63);               // -1.0842e-19
    EXPECT_EQ(besm6_to_ieee(0'0040'0000'0000'0001ull), 0x1p-103);               //  9.86076e-32
    EXPECT_EQ(besm6_to_ieee(0'0060'0000'0000'0001ull), -0x1.fffffffffep-64);    // -1.0842e-19
    EXPECT_EQ(besm6_to_ieee(0'0050'0000'0000'0000ull), 0x1p-64);                //  5.42101e-20
    EXPECT_EQ(besm6_to_ieee(0'0070'0000'0000'0000ull), -0x1p-64);               // -5.42101e-20
    EXPECT_EQ(besm6_to_ieee(0'0057'7777'7777'7777ull), 0x1.fffffffffep-64);     //  1.0842e-19
    EXPECT_EQ(besm6_to_ieee(0'0077'7777'7777'7777ull), -0x1p-103);              // -9.86076e-32
    EXPECT_EQ(besm6_to_ieee(0'4000'0000'0000'0000ull), 0.0);                    //  0.0
    EXPECT_EQ(besm6_to_ieee(0'4020'0000'0000'0000ull), -0x1p+0);                // -1.0
    EXPECT_EQ(besm6_to_ieee(0'4000'0000'0000'0001ull), 0x1p-40);                //  9.09495e-13
    EXPECT_EQ(besm6_to_ieee(0'4020'0000'0000'0001ull), -0x1.fffffffffep-1);     // -0.999...999
    EXPECT_EQ(besm6_to_ieee(0'4010'0000'0000'0000ull), 0x1p-1);                 //  0.5
    EXPECT_EQ(besm6_to_ieee(0'4030'0000'0000'0000ull), -0x1p-1);                // -0.5
    EXPECT_EQ(besm6_to_ieee(0'4017'7777'7777'7777ull), 0x1.fffffffffep-1);      //  0.999...999
    EXPECT_EQ(besm6_to_ieee(0'4037'7777'7777'7777ull), -0x1p-40);               // -9.09495e-13
    EXPECT_EQ(besm6_to_ieee(0'7740'0000'0000'0000ull), 0.0);                    //  0.0
    EXPECT_EQ(besm6_to_ieee(0'7760'0000'0000'0000ull), -0x1p+63);               // -9.22337e+18 overflow
    EXPECT_EQ(besm6_to_ieee(0'7740'0000'0000'0001ull), 0x1p+23);                //  8.38861e+06
    EXPECT_EQ(besm6_to_ieee(0'7760'0000'0000'0001ull), -0x1.fffffffffep+62);    // -9.22337e+18
    EXPECT_EQ(besm6_to_ieee(0'7750'0000'0000'0000ull), 0x1p+62);                //  4.61169e+18
    EXPECT_EQ(besm6_to_ieee(0'7770'0000'0000'0000ull), -0x1p+62);               // -4.61169e+18
    EXPECT_EQ(besm6_to_ieee(0'7757'7777'7777'7777ull), 0x1.fffffffffep+62);     //  9.22337e+18
    EXPECT_EQ(besm6_to_ieee(0'7777'7777'7777'7777ull), -0x1p+23);               // -8.38861e+06
    // clang-format on
}

//
// Conversion from IEEE to BESM-6 floating point format.
//
TEST(arith, ieee_to_besm6)
{
    // clang-format off
    //                                                2 - sign
    //                                       exponent | ___mantissa___
    //                                              /\|/              |
    EXPECT_EQ(ieee_to_besm6(0.0),                 0'0000'0000'0000'0000ull); //  0.0
    EXPECT_EQ(ieee_to_besm6(-0x1p-64),            0'0020'0000'0000'0000ull); // -5.42101e-20
    EXPECT_EQ(ieee_to_besm6(0x1p-104),            0'0000'0000'0000'0001ull); //  4.93038e-32
    EXPECT_EQ(ieee_to_besm6(-0x1.fffffffffep-65), 0'0020'0000'0000'0001ull); // -5.42101e-20
    EXPECT_EQ(ieee_to_besm6(0x1p-65),             0'0010'0000'0000'0000ull); //  2.71051e-20
    EXPECT_EQ(ieee_to_besm6(-0x1p-65),            0'0030'0000'0000'0000ull); // -2.71051e-20
    EXPECT_EQ(ieee_to_besm6(0x1.fffffffffep-65),  0'0017'7777'7777'7777ull); //  5.42101e-20
    EXPECT_EQ(ieee_to_besm6(-0x1p-104),           0'0037'7777'7777'7777ull); // -4.93038e-32
                                                                             //  0.0
    EXPECT_EQ(ieee_to_besm6(-0x1p-63),            0'0060'0000'0000'0000ull); // -1.0842e-19
    EXPECT_EQ(ieee_to_besm6(0x1p-103),            0'0040'0000'0000'0001ull); //  9.86076e-32
    EXPECT_EQ(ieee_to_besm6(-0x1.fffffffffep-64), 0'0060'0000'0000'0001ull); // -1.0842e-19
    EXPECT_EQ(ieee_to_besm6(0x1p-64),             0'0050'0000'0000'0000ull); //  5.42101e-20
    // 0070'0000'0000'0000 is the same as 0020'0000'0000'0000 above          // -5.42101e-20
    EXPECT_EQ(ieee_to_besm6(0x1.fffffffffep-64),  0'0057'7777'7777'7777ull); //  1.0842e-19
    EXPECT_EQ(ieee_to_besm6(-0x1p-103),           0'0077'7777'7777'7777ull); // -9.86076e-32
                                                                             //  0.0
    EXPECT_EQ(ieee_to_besm6(-0x1p+0),             0'4020'0000'0000'0000ull); // -1.0
    EXPECT_EQ(ieee_to_besm6(0x1p-40),             0'4000'0000'0000'0001ull); //  9.09495e-13
    EXPECT_EQ(ieee_to_besm6(-0x1.fffffffffep-1),  0'4020'0000'0000'0001ull); // -0.999...999
    EXPECT_EQ(ieee_to_besm6(0x1p-1),              0'4010'0000'0000'0000ull); //  0.5
    EXPECT_EQ(ieee_to_besm6(-0x1p-1),             0'4030'0000'0000'0000ull); // -0.5
    EXPECT_EQ(ieee_to_besm6(0x1.fffffffffep-1),   0'4017'7777'7777'7777ull); //  0.999...999
    EXPECT_EQ(ieee_to_besm6(-0x1p-40),            0'4037'7777'7777'7777ull); // -9.09495e-13
                                                                             //  0.0
    EXPECT_EQ(ieee_to_besm6(-0x1p+63),            0'7760'0000'0000'0000ull); // -9.22337e+18 overflow
    EXPECT_EQ(ieee_to_besm6(0x1p+23),             0'7740'0000'0000'0001ull); //  8.38861e+06
    EXPECT_EQ(ieee_to_besm6(-0x1.fffffffffep+62), 0'7760'0000'0000'0001ull); // -9.22337e+18
    EXPECT_EQ(ieee_to_besm6(0x1p+62),             0'7750'0000'0000'0000ull); //  4.61169e+18
    EXPECT_EQ(ieee_to_besm6(-0x1p+62),            0'7770'0000'0000'0000ull); // -4.61169e+18
    EXPECT_EQ(ieee_to_besm6(0x1.fffffffffep+62),  0'7757'7777'7777'7777ull); //  9.22337e+18
    EXPECT_EQ(ieee_to_besm6(-0x1p+23),            0'7777'7777'7777'7777ull); // -8.38861e+06
    // clang-format on
}
