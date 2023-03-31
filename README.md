![build status](https://github.com/besm6/dubna/actions/workflows/c-cpp.yml/badge.svg)

The goal of this project is to simulate the Dubna monitor system as described in the book
[Программирование на БЭСМ-6 в системе "Дубна"](https://www.google.com/books/edition/%D0%9F%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5_%D0%BD%D0%B0_%D0%91/oVMWzgEACAAJ)
by Gennady Maznyi.

# Build

Compile the Dubna simulator from sources and install into /usr/local:

```
make
make install
```

Run tests:
```
make test
```
Expected output:
```
ctest --test-dir build/tests
...
 1/42 Test  #1: cli.usage ........................   Passed    0.01 sec
      Start  2: cli.version
 2/42 Test  #2: cli.version ......................   Passed    0.01 sec
      Start  3: cli.trace_end_file
...
42/42 Test #42: unit.encode_cosy .................   Passed    0.00 sec

100% tests passed, 0 tests failed out of 42

Total Test time (real) =   0.21 sec
```

# Examples

A few demos are available in the `examples` directory:

```
$ cd examples
$ dubna name.dub
Read job 'name.dub'
Mount image '/Users/vak/.besm6/9' as disk 30
Redirect drum 21 to disk 30
------------------------------------------------------------


                                             3  000    00.00
 ЙОКСЕЛ      БЭСМ-6/5     ШИФР-12
 МОНИТОРНАЯ СИСТЕМА  ′Д У Б Н А′  -  20/10/88







            ЖЖЖЖЖ ЖЖЖЖ  Ж   Ж Ж   Ж ЖЖЖЖ  ЖЖЖЖ
            Ж   Ж Ж   Ж Ж   Ж ЖЖ ЖЖ Ж     Ж   Ж
            Ж   Ж Ж   Ж Ж   Ж Ж Ж Ж ЖЖЖ   Ж   Ж
            Ж   Ж Ж   Ж Ж  ЖЖ Ж Ж Ж Ж     Ж   Ж
            Ж   Ж ЖЖЖЖ  Ж Ж Ж Ж   Ж Ж     ЖЖЖЖ
            Ж   Ж Ж     ЖЖ  Ж Ж   Ж Ж     Ж
            Ж   Ж Ж     Ж   Ж Ж   Ж ЖЖЖЖЖ Ж








*NАМЕ ПРИМЕР
*ЕND FILЕ
------------------------------------------------------------
   Elapsed time: 0.007 seconds
      Simulated: 209161 instructions
Simulation rate: 30269320 instructions/sec
```
