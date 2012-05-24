#!/bin/bash 

function bnfct {
 Run=`mkdir -p tmp; cd tmp; cp ../input/$1.* .; bnfc -m $1.cf; make; cat ../input/$1.test | ./Test;  cd ..; rm -rf tmp`
 ExpectedOutput=`cat ../haskell/output/$1.out`
 assertEquals "${ExpectedOutput}" "${Run}"
}

testBNFC_lbnf(){
    bnfct lbnf
}

testBNFC_lbnf2(){
    bnfct lbnf
}

. ./shunit2
