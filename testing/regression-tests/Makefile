all:
	happy -gca Par110_backslash.y
	alex -g Lex110_backslash.x
	ghc --make Test110_backslash.hs -o Test110_backslash

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Doc110_backslash.ps

distclean: clean
	-rm -f Doc110_backslash.* Lex110_backslash.* Par110_backslash.* Layout110_backslash.* Skel110_backslash.* Print110_backslash.* Test110_backslash.* Abs110_backslash.* Test110_backslash ErrM.* SharedString.* ComposOp110_backslash.* 110_backslash.dtd XML110_backslash.* Makefile*
	

