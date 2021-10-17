default :
	make -C source
	make -C testing

testing : install test

install :
	make -C source install

internal-tests :
	make -C source test

test :
	cd testing && cabal v1-install && bnfc-system-tests && cd ..

tag :
	hasktags --etags ./source -o ./source/TAGS
#EOF
