#!/bin/sh
#
# Create images of tapes in SIMH format:
#   * tape 9/monsys: file "monsys.9" is converted to "9"
#   * tape 12/librar: file "librar.12" is converted to "12"
#   * tape 37/librar: file "librar.37" is converted to "37"
#
set -x

#----------------------------------------------------------------------
# Tape 9/monsys
#

# Create copy of file monsys.9 and apply patches.
cp monsys.9 monsys.9.patched
chmod +w monsys.9.patched
(
    echo "08ffee: 70 90 1b 70 a0 1c"
    echo "08fff4: 7b 7b 55 ec c4 ae"
    echo "08fffa: 70 80 18 90 d0 00"
) | xxd -r - monsys.9.patched

# Dump in text format for manual inspection.
xxd -c6 -g1 monsys.9 > monsys.9.xd
xxd -c6 -g1 monsys.9.patched > monsys.9.patched.xd

# Create new SIMH image of tape "9".
rm -f ~/.besm6/9
touch ~/.besm6/9
besmtool write 9 --from-file=monsys.9.patched
cp ~/.besm6/9 9

# Dump in text format for manual inspection.
xxd -c6 -g1 9 > 9.xd

#----------------------------------------------------------------------
# Tape 12/librar
#

# Create new SIMH image of tape "12".
rm -f ~/.besm6/12
touch ~/.besm6/12
besmtool write 12 --from-file=librar.12
cp ~/.besm6/12 12

# Dump in text format for manual inspection.
xxd -c6 -g1 12 > 12.xd

#----------------------------------------------------------------------
# Tape 37/librar
#

# Create new SIMH image of tape "37".
rm -f ~/.besm6/37
touch ~/.besm6/37
besmtool write 37 --from-file=librar.37
cp ~/.besm6/37 37

# Dump in text format for manual inspection.
xxd -c6 -g1 37 > 37.xd
