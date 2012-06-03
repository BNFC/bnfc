#!/bin/bash

for LANG in "lbnf" "gf" "cpp"; do 

    for BACKEND in "haskell" "ocaml" "c" "cpp" "java"; do 
	echo "bnfc -${BACKEND} -m ${LANG}/${LANG}.cf"
	bnfc -${BACKEND} -m ${LANG}/${LANG}.cf > /dev/null 2>&1
	make > /dev/null 2>&1
	if  [ $BACKEND == "java" ]; then
	    echo "cat ${LANG}/${LANG}.test | java ${LANG}.Test > ../output/${BACKEND}/${LANG}_parse.out"
	    RESULT=`cat ${LANG}/${LANG}.test | java ${LANG}.Test`
	else
	    echo "cat ${LANG}/${LANG}.test | ./Test${LANG} > ../output/${BACKEND}/${LANG}_parse.out"
	    RESULT=`cat ${LANG}/${LANG}.test | ./Test${LANG}`
	fi
	
	if [ -z "${RESULT}" ]; then
	    echo "FAIL" > ../output/${BACKEND}/${LANG}_parse.out
	else
	    echo "${RESULT}" > ../output/${BACKEND}/${LANG}_parse.out
	fi
	
	make distclean  > /dev/null 2>&1
    done

done
