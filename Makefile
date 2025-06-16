# adapted from https://github.com/STAT545-UBC/STAT545-UBC.github.io

RMDHTML ::= $(patsubst %.Rmd, %.html, $(wildcard *.Rmd))
RMDR ::= $(patsubst %.Rmd, %.R, $(wildcard *.Rmd))
CLASSDIR = linux.stat.uiowa.edu:/homepage/luke/SIBS-R/SIBS-WV-2025

all: $(RMDHTML) $(RMDR)
	cd slides; $(MAKE)
	cd ICON; $(MAKE)

web: all
	rsync -avz --chmod=a+r,Da+x . $(CLASSDIR)/

# Patterns

%.html: %.Rmd _output.yaml include/nav.html
	Rscript -e 'rmarkdown::render("$<")'

%.R: %.Rmd
	Rscript -e 'knitr::purl("$<")'

