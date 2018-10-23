RFILE = README

all: knitr

knitr: $(RFILE).Rmd
	echo "rmarkdown::render('$(RFILE).Rmd',rmarkdown::md_document(variant='gfm'))" | R --no-save -q

clean:
	rm -rf *.md
