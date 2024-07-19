Files in this directory:

  * monsys.9  - Raw image of MS Dubna installation tape
  * librar.12 - Raw image of Common Software Library #1 tape
  * librar.37 - Raw image of Common Software Library #2 tape
  * 9         - Tape 9/monsys
  * 12        - Tape 12/librar
  * 37        - Tape 37/librar
  * 739       - Tape 739/dispac with Bemsh
  * lib1.txt  - Contents of Common Software Library #1
  * lib2.txt  - Contents of Common Software Library #2

For details, see: [besm6.github.io/tree/master/sources/dubna](https://github.com/besm6/besm6.github.io/tree/master/sources/dubna)

Images 9, 12 and 37 in this directory were created from files
monsys.9, librar.12 and librar.37 respectively.

    $ besmtool write 9 --from-file=monsys.9
    Written 288 zones (1728 kbytes) from file monsys.9 to disk 9

    $ besmtool write 12 --from-file=librar.12
    Written 320 zones (1920 kbytes) from file librar.12 to disk 12

    $ besmtool write 37 --from-file=librar.37
    Written 424 zones (2544 kbytes) from file librar.37 to disk 37

Image 739 in this directory was created from disk 2048 of dispak package.

    $ besmtool write 739 --from-disk=2048 --from-start=0144 --length=034
    Writing 28 zones (168 kbytes) from disk 2048/0144 to disk 739/0000
