# adapted from https://github.com/STAT545-UBC/STAT545-UBC.github.io

RMDHTML ::= $(patsubst %.Rmd, %.html, $(wildcard *.Rmd))
RMDR ::= $(patsubst %.Rmd, %.R, $(wildcard *.Rmd))

all: $(RMDHTML) $(RMDR)

# Patterns

%.html: %.Rmd _output.yaml include/nav.html
	Rscript -e 'rmarkdown::render("$<")'

%.R: %.Rmd
	Rscript -e 'knitr::purl("$<")'

