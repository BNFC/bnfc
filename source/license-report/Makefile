# Andreas Abel, 2021-01-04
#
# Generate license-report for bnfc using cabal-plan.

.PHONY: default index.md
default: index.html

index.md :
	cabal-plan license-report BNFC:exe:bnfc > $@

%.html : %.md
	pandoc -s -t html -o $@ $<

# EOF
