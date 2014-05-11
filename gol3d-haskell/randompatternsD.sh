#!/usr/bin/zsh

# A script to generate a bunch of patterns with a given density in a given bounding box using the DensityGen program.

if (( $# < 5 ))
then
    echo "Insufficient parameters." >&2
    echo "  usage: makerandomsD.sh <x> <y> <z> <n> <step>" >&2
    echo "  x, y, z define the bounding box." >&2
    echo "  n defines the number patterns to make, and step is which densities to test." >&2
    echo "  For example, 'makerandomsD.sh 10 10 10 5 0.2' will produce, in a 10x10x10 box," >&2
    echo "  5 random patterns for each density 0.2, 0.4, 0.6, and 0.8." >&2
    exit 1
fi

x=$1
y=$2
z=$3
n=$4
s=$5

odir="$x,$y,${z}--${n}-${s}"

mkdir -p $odir

for (( d=$s ; d < 1.0; d+=$s ))  
do 
    for (( i=0; i < $n; i++ )) 
    do
        ./DensityGen $d $x $y $z > "$odir/${d}-${i}.pat"
    done
done
