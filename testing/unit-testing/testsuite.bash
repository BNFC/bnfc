#!/bin/bash 

function parse_test {
 mkdir -p tmp; cd tmp
 cp ../input/$1/$1.cf .
 if [ $2 == "haskell" ]; then
     bnfc -m $1.cf > /dev/null
 elif [ $2 == "c" ]; then
     bnfc -c -m $1.cf > /dev/null
 fi
 make 2>1 > /dev/null
 Run=`cat ../input/$1/$1.test | ./Test$1`
 ExpectedOutput=`cat ../output/$2/$1_parse.out`
 cd .. ; rm -rf tmp
 assertEquals "${ExpectedOutput}" "${Run}"
}

test_haskell_lbnf_parse_test(){
    parse_test lbnf haskell
}

test_haskell_gf_parse_test(){
    parse_test gf haskell
}

test_c_lbnf_parse_test(){
    parse_test lbnf c
}

test_c_gf_parse_test(){
    parse_test gf c
}

. ./shunit2
