#!/bin/bash

if [[ -z "$1" ]]; then
    echo "Usage: ./encodings.sh test{n} [n]"
    exit 1
fi

testfile=$1

pstest=${testfile}ps.lpod
l2atest=${testfile}l2a.lp
ourtest=${testfile}.lpod
tempfile=${testfile}temp

../build/lpodstats $ourtest # outputs $ourtest.stats
read r odr totat at maxn prodn < ${testfile}.stats

../build/lpod2asp1 $ourtest
clingo $testfile.lp 0 > $tempfile 2>/dev/null
t1=$(grep "CPU Time" $tempfile | awk '{print $4}' | sed 's/s//')

../build/lpod2asp2 $ourtest
asprin $testfile.lp ../pref.lp 0 > $tempfile 2>/dev/null
t2=$(grep "CPU Time" $tempfile | awk '{print $4}' | sed 's/s//')

../build/lpod2asp3 $ourtest
asprin $testfile.lp ../pref.lp 0 > $tempfile 2>/dev/null
t3=$(grep "CPU Time" $tempfile | awk '{print $4}' | sed 's/s//')

../build/lpod2asp4 $ourtest
clingo $testfile.lp 0 > $tempfile 2>/dev/null
cand=$(grep "Models" $tempfile | awk '{print $3}')
asprin $testfile.lp ../pref.lp 0 > $tempfile 2>/dev/null
t4=$(grep "CPU Time" $tempfile | awk '{print $4}' | sed 's/s//')
pref=$(grep "Optimal" $tempfile | awk '{print $3}')

echo "P$2,$at,$r,$odr,$maxn,$cand,$pref,$t1,$t2,$t3,$t4"
rm $tempfile $testfile.lp ${testfile}.stats
