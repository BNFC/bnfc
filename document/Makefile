all : LBNF-report.pdf

%.pdf : %.tex
	latexmk -pdf $<

CC-2004:
	latex CC-2004
	latex CC-2004
	bibtex CC-2004
	latex CC-2004
	latex CC-2004
	dvips CC-2004.dvi -o CC-2004.ps
	gv CC-2004.ps &
tooldemo:
	latex HW-tooldemo
	latex HW-tooldemo
	bibtex HW-tooldemo
	latex HW-tooldemo
	latex HW-tooldemo
	dvips -P cmz -t letter HW-tooldemo.dvi -o HW-tooldemo.ps
	gv HW-tooldemo.ps &

simple:
	latex CC-2004
clean:
	rm *.log *.dvi *.aux *.bbl *.blg
ps:
	dvips CC-2004.dvi -o CC-2004.ps
spell:
	ispell -d american CC-2004.tex
