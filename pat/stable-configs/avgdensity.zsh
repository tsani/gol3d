#!/usr/bin/zsh

# script to calculate the average density of all these patterns

sum=0  
N="$(wc -l densities.txt | awk '{ print "$1" }').0"
awk ' { print $2 } ' < densities.txt | while read d 
do 
    let "sum+=$d"  
done 
echo $(( $sum ))
