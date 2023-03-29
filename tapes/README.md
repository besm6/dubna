Files in this directory:

  * monsys.9  - Raw image of MS Dubna installation tape
  * librar.12 - Raw image of Common Software Library #1 tape
  * librar.37 - Raw image of Common Software Library #2 tape
  * 9         - Disk image of monsys.9 for SIMH/dispak
  * 12        - Disk image of librar.12 for SIMH/dispak
  * 37        - Disk image of librar.37 for SIMH/dispak
  * lib1.txt  - Contents of Common Software Library #1
  * lib2.txt  - Contents of Common Software Library #2

For details, see: [besm6.github.io/tree/master/sources/dubna](https://github.com/besm6/besm6.github.io/tree/master/sources/dubna)

Disk images 9, 12 and 37 in this directory were created from files
monsys.9, librar.12 and librar.37 respectively.

    $ besmtool write 9 --from-file=monsys.9
    Written 288 zones (1728 kbytes) from file monsys.9 to disk 9

    $ besmtool write 12 --from-file=librar.1
    Written 320 zones (1920 kbytes) from file librar.12 to disk 12

    $ besmtool write 37 --from-file=librar.37
    Written 424 zones (2544 kbytes) from file librar.37 to disk 37
