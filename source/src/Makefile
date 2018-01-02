# Author: Andreas Abel, 2018-01-01

# bnfc's own parser
###################

# BNFC-generated files
gen = AbsBNF.hs DocBNF.txt ErrM.hs LexBNF.x ParBNF.y PrintBNF.hs SkelBNF.hs TestBNF.hs

all : AbsBNF.hs LexBNF.hs ParBNF.hs

Abs%.hs Doc%.txt Lex%.x Par%.y Print%.hs Skel%.hs Test%.hs : %.cf
	bnfc --haskell --ghc $<

%.hs : %.x
	alex $<

%.hs : %.y
	happy -i $<

# Emacs Tags
############

.PHONY : tags

tags :
	hasktags -e .

#EOF
