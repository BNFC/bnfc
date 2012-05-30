#!/bin/bash 

function parse_test {
 mkdir -p tmp; cd tmp
 cp ../input/$1/$1.cf .
 if [ $2 == "haskell" ]; then
     echo " bnfc -m $1.cf"
     bnfc -m $1.cf > /dev/null
 elif [ $2 == "c" ]; then
     echo " bnfc -c -m $1.cf"
     bnfc -c -m $1.cf > /dev/null
 elif [ $2 == "cpp" ]; then
     echo " bnfc -cpp -m $1.cf"
     bnfc -cpp -m $1.cf > /dev/null
 elif [ $2 == "ocaml" ]; then
     echo " bnfc -ocaml -m $1.cf"
     bnfc -ocaml -m $1.cf > /dev/null
 fi
 make 2>1 > /dev/null
 echo " cat ../input/$1/$1.test | ./Test$1"
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

# test_cpp_lbnf_parse_test(){
#    parse_test lbnf cpp
#}

#test_cpp_gf_parse_test(){
#    parse_test gf cpp
#}

test_ocaml_lbnf_parse_test(){
    parse_test lbnf ocaml
}

test_ocaml_gf_parse_test(){
    parse_test gf ocaml
}


. ./shunit2
