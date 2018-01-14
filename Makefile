default : install internal-tests test

install :
	cd source && cabal install && cd ..

internal-tests :
	cd source && cabal configure --enable-tests && cabal test && cd ..

test :
	cd testing && cabal install && bnfc-system-tests && cd ..

#EOF
