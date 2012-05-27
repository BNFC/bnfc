#!/bin/bash 

function bnfc_haskell_parse_test {
 mkdir -p tmp; cd tmp
 cp ../input/$1/$1.cf .
 bnfc -m $1.cf > /dev/null
 make 2>1 > /dev/null
 Run=`cat ../input/$1/$1.test | ./Test$1`
 ExpectedOutput=`cat ../haskell/output/$1.out`
 cd .. ; rm -rf tmp
 assertEquals "${ExpectedOutput}" "${Run}"
}

test_lbnf_haskell_parse_test(){
    bnfc_haskell_parse_test lbnf
}

test_gf_haskell_parse_test(){
    bnfc_haskell_parse_test gf
}

. ./shunit2
