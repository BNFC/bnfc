NAME=GPCE-2004

all:
	latex $(NAME)
	latex $(NAME)
	bibtex $(NAME)
	latex $(NAME)
	latex $(NAME)
	dvips $(NAME).dvi -o $(NAME).ps
	gv $(NAME).ps &
simple:
	latex $(NAME)
clean:
	rm *.log *.dvi *.aux *.bbl *.blg *~
ps:
	dvips $(NAME).dvi -o $(NAME).ps
spell:
	ispell -d american $(NAME).tex
