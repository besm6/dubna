#!/bin/sh
for f in *.txt
do
    echo $f
    sed -i~ -f cleanup.sed $f
done
rm -f *~
