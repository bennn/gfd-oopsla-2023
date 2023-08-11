all: full

full:	paper.tex bib.bib
	pdflatex paper
	bibtex paper
	pdflatex paper
	pdflatex paper

fast: paper.tex bib.bib
	pdflatex paper.tex

