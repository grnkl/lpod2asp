#!/bin/bash

if [[ -z "$1" ]]; then
    echo "Usage: ./systems.sh test{n} [n]"
    exit 1
fi

testfile=$1

pstest=${testfile}ps.lpod
l2atest=${testfile}l2a.lp
ourtest=${testfile}.lpod
tempfile=${testfile}temp

../build/lpodstats $ourtest # outputs $ourtest.stats
read r odr totat at maxn prodn < ${testfile}.stats

# the lparse and psmodels executables must be placed in the same directory as this script
./lparse --inclusion-optimal $pstest > $tempfile 2> /dev/null
t0=$(date +%s%N)
./psmodels 0 $tempfile > /dev/null 2> /dev/null
t1=$(date +%s%N)
ps=$(echo "scale=3; ($t1- $t0) / 1000000000" | bc)

# lpod2asprin files must be placed in the same directory as this script
python ./lpod2asprin.py -i $l2atest -type i > $tempfile 2>/dev/null
l2a=$(grep "CPU Time" $tempfile | awk '{print $4}' | sed 's/s//')

../build/lpod2asp3 $ourtest
asprin $testfile.lp ../pref.lp 0 > $tempfile 2>/dev/null
t3=$(grep "CPU Time" $tempfile | awk '{print $4}' | sed 's/s//')

../build/lpod2asp4 $ourtest
clingo $testfile.lp 0 > $tempfile 2>/dev/null
cand=$(grep "Models" $tempfile | awk '{print $3}')
asprin $testfile.lp ../pref.lp 0 > $tempfile 2>/dev/null
t4=$(grep "CPU Time" $tempfile | awk '{print $4}' | sed 's/s//')
pref=$(grep "Optimal" $tempfile | awk '{print $3}')

echo "P$2,$at,$r,$odr,$maxn,$cand,$pref,$ps,$l2a,$t3,$t4"
rm $tempfile $testfile.lp ${testfile}.stats
