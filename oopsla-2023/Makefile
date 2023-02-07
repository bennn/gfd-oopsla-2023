all: full

full:	paper.tex bib.bib
	pdflatex paper && \
	bibtex paper && \
	pdflatex paper && \
	pdflatex paper && \
	echo "done, fully" 

fast: paper.tex bib.bib
	pdflatex paper.tex && \
	open paper.pdf && \
	echo "done"

