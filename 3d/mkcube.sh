#!/usr/bin/zsh

X=$1
Y=$2
Z=$3

for (( i=-X/2; i<X/2; i++ ))
do
    for (( j=-Y/2; j<Y/2; j++ ))
    do
        for (( k=-Z/2; k<Z/2; k++ ))
        do
            echo $i $j $k
        done
    done
done | sort | uniq
