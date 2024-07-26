#!/bin/bash

err_msg="Usage: ./lpod2asp.sh example.lpod [-enc i] where i = 1,2,3,4"

if [ "$#" -lt 1 ]; then
    echo "$err_msg"
    exit 1
fi

srcname="${1%.*}"

i=4 # default encoding

if [ "$#" -eq 3 ]; then
    i="$3"
fi

if ! [[ "$i" =~ ^[1-4]$ ]]; then
    echo "$err_msg"
    exit 1
fi

./build/lpod2asp${i} "$srcname.lpod" # outputs $srcname.lp

if [ "$i" -eq 1 ]; then
    clingo "$srcname.lp" 0 2>/dev/null
else
    asprin "$srcname.lp" pref.lp 0 2>/dev/null
fi

rm $srcname.lp