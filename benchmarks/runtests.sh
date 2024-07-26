#!/bin/bash

echo "test,atoms,rules,od rules,max od,candidate,preferred,t1,t2,t3,t4"
for i in {01..09}; do
    testfile="./tests/P${i}/test${i}"
    ./encodings.sh $testfile $i
done


echo -e "\ntest,atoms,rules,od rules,max od,candidate,preferred,psmodels,lpod2asprin,t3,t4"
for i in {10..16}; do
    testfile="./tests/P${i}/test${i}"
    ./systems.sh $testfile $i
done
