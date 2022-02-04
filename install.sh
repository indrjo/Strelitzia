#!/usr/bin/env sh

# This tiny script uniquely serves to compile the file `strelitzia.hs` into
# an executable located inside some directory in the PATH. By default, the
# directory "~/.local/bin" is chosed. Two are the reasons for that feature:
# no particular privilege of administration is required to locate things
# there and it is likely to be already present in your PATH. Remember:
# if you want to keep the default, and "~/.local/bin" is not in your PATH,
# then you must add it. However, if you want to change such behaviour, just
# edit the following line.
here=$HOME/.local/bin

[ -d $here ] || mkdir -p $here

# Now, start the compilation of the program.
ghc -Wall -O2 -fforce-recomp -o $here/strelitzia ./strelitzia.hs
