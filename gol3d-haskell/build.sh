#!/usr/bin/zsh

if [[ $1 = "clean" ]]
then 
    rm *.o *.hi
    exit 0
fi

for f in *.hs 
do
    ghc --make -O2 -odir bin $f
done
