#!/bin/bash 

function parse_test {
 mkdir -p tmp; cd tmp
 cp ../input/$1/$1.cf .
 bnfc -m $1.cf > /dev/null
 make 2>1 > /dev/null
 Run=`cat ../input/$1/$1.test | ./Test$1`
 ExpectedOutput=`cat ../output/haskell/$1_parse.out`
 cd .. ; rm -rf tmp
 assertEquals "${ExpectedOutput}" "${Run}"
}

test_haskell_lbnf_parse_test(){
    parse_test lbnf
}

test_haskell_gf_parse_test(){
    parse_test gf
}

. ./shunit2
