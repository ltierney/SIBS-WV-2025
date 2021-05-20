# adapted from https://github.com/STAT545-UBC/STAT545-UBC.github.io

RMDHTML ::= $(patsubst %.Rmd, %.html, $(wildcard *.Rmd))

all: $(RMDHTML)

# Patterns

%.html: %.Rmd
	Rscript -e 'rmarkdown::render("$<")'

%.html: %.md
	Rscript -e 'rmarkdown::render("$<")'
