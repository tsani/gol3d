#!/usr/bin/zsh

#A simple script for checking a bunch of files for spaceships. Not very fast because of the JVM spawns.

for f in $@
do
    if ( echo "test cycle" | ./run.sh -i "$f" -s - ) 
    then
        cp "$f" cycles/
        echo "Found cycle"
    fi
done
