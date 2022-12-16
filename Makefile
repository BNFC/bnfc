default :
	make -C source
	make -C testing

testing : install test

install :
	make -C source install

internal-tests :
	make -C source test

test :
	make -C testing test

#EOF
