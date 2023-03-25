//
// Arithmetic tests.
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
#include "fixture_machine.h"

//
// Test: multiplication in ALU.
//
TEST_F(dubna_machine, alu_mul)
{
    // Disable normalization
    machine->set_rau(3);

    machine->set_acc(04050000000000000);               // 1/2 * 2^1
    machine->arith_multiply(04050000000000000);        // 1/2 * 2^1
    EXPECT_EQ(machine->get_acc(), 04104000000000000u); // 1/4 * 2^2
    EXPECT_EQ(machine->get_rmr(), 0u);

    machine->set_acc(04050000000000000);               // 1/2 * 2^1
    machine->arith_multiply(04020000000000000);        // -1 * 2^0
    EXPECT_EQ(machine->get_acc(), 04070000000000000u); // -1/2 * 2^1
    EXPECT_EQ(machine->get_rmr(), 0u);

    machine->set_acc(04020000000000000);               // -1 * 2^0
    machine->arith_multiply(04050000000000000);        // 1/2 * 2^1
    EXPECT_EQ(machine->get_acc(), 04070000000000000u); // -1/2 * 2^1
    EXPECT_EQ(machine->get_rmr(), 0u);

    machine->set_acc(04020000000000000);               // -1 * 2^0
    machine->arith_multiply(04020000000000000);        // -1 * 2^0
    EXPECT_EQ(machine->get_acc(), 04050000000000000u); // 1/2 * 2^1
    EXPECT_EQ(machine->get_rmr(), 0u);
}

//
// Test: division in ALU.
//
TEST_F(dubna_machine, alu_div)
{
    // Disable normalization
    machine->set_rau(3);

    // 1.0 / 1.0 gives 1.0
    machine->set_acc(04050000000000000);               // 1/2 * 2^1
    machine->arith_divide(04050000000000000);          // 1/2 * 2^1
    EXPECT_EQ(machine->get_acc(), 04050000000000000u); // 1/2 * 2^1
    EXPECT_EQ(machine->get_rmr(), 0u);

    // -1.0 / -1.0 gives 1.0
    machine->set_acc(04020000000000000);               // -1 * 2^0
    machine->arith_divide(04020000000000000);          // -1 * 2^0
    EXPECT_EQ(machine->get_acc(), 04050000000000000u); // 1/2 * 2^1
    EXPECT_EQ(machine->get_rmr(), 0u);

    // 1.0 / -1.0 gives -1.0 denormalized
    machine->set_acc(04050000000000000);               // 1/2 * 2^1
    machine->arith_divide(04020000000000000);          // -1 * 2^0
    EXPECT_EQ(machine->get_acc(), 04070000000000000u); // -1/2 * 2^1
    EXPECT_EQ(machine->get_rmr(), 0u);

    // -1.0 / 1.0 gives -1.0
    machine->set_acc(04020000000000000);               // -1 * 2^0
    machine->arith_divide(04050000000000000);          // 1/2 * 2^1
    EXPECT_EQ(machine->get_acc(), 04020000000000000u); // -1 * 2^0
    EXPECT_EQ(machine->get_rmr(), 0u);
}
