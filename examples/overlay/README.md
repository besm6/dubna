Programs can be linked as static binaries, using so called 'overlay' format.
These binaries can be invoked directly, without a need for the moninoring system.
In this directory you can find examples of 'Hello World' for different languages.

  * hello-algol.dub   - Algol-ГДP (7.01.82)
  * hello-assem.dub   - Assembler Madlen (1.10.72)
  * hello-bemsh.dub   - Assembler БЭМШ (06/78)
  * hello-forex.dub   - Forex (11.09.85)
  * hello-fortran.dub - Fortran Dubna (16.07.73)
  * hello-ftn.dub     - Fortran-ГДP (09.07.81)
  * hello-madlen.dub  - Assembler Madlen-3.5 (23/06/79)
  * hello-pascal.dub  - Pascal (24.12.79)

# Compile

To compile the sources into a binary file 'hello.bin', run the appropriate source
file, for example:

```
$ dubna hello-pascal.dub
                                             18 HOЯ 24 13.39
 ЙOKCEЛ      БЭCM-6/5     ШИФP-12
 MOHИTOPHAЯ CИCTEMA  ′Д Y Б H A′  -  20/10/88
[...]
≠
 ДЛИHA БИБЛИOTEKИ  002 17
*END FILE
```

# Run

Invoke the binary program:
```
$ dubna hello.bin
HELLO, PASCAL!
```
