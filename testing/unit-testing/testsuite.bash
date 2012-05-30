#!/bin/bash 

function parse_test {
    mkdir -p tmp; cd tmp
    cp ../input/$1/$1.cf .
    echo " bnfc -$2 -m $1.cf"
    bnfc -$2 -m $1.cf > /dev/null
    make 2>1 > /dev/null
    if  [ $2 == "java" ]; then
	echo " cat ../input/$1/$1.test | java $1/Test"
	Run=`cat ../input/$1/$1.test | java $1/Test`
    else
	echo " cat ../input/$1/$1.test | ./Test$1"
	Run=`cat ../input/$1/$1.test | ./Test$1`
    fi
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

test_ocaml_lbnf_parse_test(){
    parse_test lbnf ocaml
}

test_ocaml_gf_parse_test(){
    parse_test gf ocaml
}

test_c_lbnf_parse_test(){
    parse_test lbnf c
}

test_cpp_lbnf_parse_test(){
    parse_test lbnf cpp
}

test_java_lbnf_parse_test(){
    parse_test lbnf java
}


. ./shunit2
