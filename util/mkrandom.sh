#!/usr/bin/zsh

SIZE="$1"
RANGE="$2"

for (( i=0; i < $SIZE; i++ ))
do
    echo $(( $RANDOM % $RANGE - ($RANGE / 2) )) $(( $RANDOM % $RANGE - ($RANGE / 2) )) $(( $RANDOM % $RANGE - ($RANGE / 2) ))
done
