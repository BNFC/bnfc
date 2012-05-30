#!/bin/bash

LANG=lbnf

for X in "haskell" "ocaml" "c" "cpp" "java"; do 
    echo "bnfc -$X -m $LANG.cf"
    bnfc -$X -m $LANG.cf > /dev/null 2>&1
    make > /dev/null 2>&1
    if  [ $X == "java" ]; then
	echo "cat $LANG.test | java lbnf.Test > ../../output/$X/${LANG}_parse.out"
	cat $LANG.test | java lbnf.Test > ../../output/$X/${LANG}_parse.out
    else
	echo "cat $LANG.test | ./Testlbnf > ../../output/$X/${LANG}_parse.out"
	cat $LANG.test | ./Testlbnf > ../../output/$X/${LANG}_parse.out
    fi
    make distclean  > /dev/null 2>&1
done
