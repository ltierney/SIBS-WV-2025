# adapted from https://github.com/STAT545-UBC/STAT545-UBC.github.io

MDHTML ::= $(patsubst %.md, %.html, $(wildcard *.md))

all: $(MDHTML)

# Patterns

%.html: %.md
	Rscript -e 'rmarkdown::render("$<")'
