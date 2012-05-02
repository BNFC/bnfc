# Makefile for running tests
# 
# At the moment, a test consists of: running bnfc on the GF, LBNF and OCL 
# .cf-grammars, running the generated Makefile, and then running the 
# test-program for parsing example GF, LBNF and OCL files (one of each).
# You have to look at the output yourself, there is no formal report of success 
# or failure of the test.
#
# Also, there are currently only rules for the ocaml and haskell backend, i.e. 
# you do a 'make haskell' or 'make ocaml'.


testone = cd $(1); ../../source/bin/bnfc -m -$(2) $(3).cf; make; ./Test$(3) $(4); cd ..;

test-ocl = $(call testone,ocl,$(1),SurfaceOCL,OwnerPIN.ocl)
test-gf = $(call testone,gf,$(1),GF,SyntaxRomance.gf)
test-lbnf = $(call testone,lbnf,$(1),BNF,BNF.cf)



langs = gf lbnf ocl

.PHONY: ocaml haskell

ocaml:
	$(call test-gf,ocaml)
	$(call test-lbnf,ocaml)
	$(call test-ocl,ocaml)
haskell: 
	$(call test-gf,haskell)
	$(call test-lbnf,haskell)
	$(call test-ocl,haskell)



#test = $(call lbnf,($1))\n\t $(call gf,($1)\n\t $(call ocl,($1)) 
#	$(foreach l,$(langs),$(call $(l),ocaml))
#	$(call test,ocaml)
