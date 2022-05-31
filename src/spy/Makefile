MAKEFLAGS += --silent
SHELL:= /bin/bash#
R=$(shell git rev-parse --show-toplevel)

help : Makefile
	echo ""; echo "usage: "
	@gawk 'BEGIN {FS="[ \t]*:.*##[ \t]*"}  \
	  NF==2 { printf \
           "  \033[36m%-15s\033[0m %s\n","make " $$1,$$2}'  $< \
	| grep -v gawk

c=spy
docs/%.html:  %.lua ## make doc
	docco -l classic $<
	cp $R/etc/docco.css docs/


../../docs/%.pdf : %.lua  ## make pdf
	@echo "pdf-ing $@ ... "
	@a2ps -Bjr    -q                        \
		-L 120 \
		--line-numbers=1                    \
		--highlight-level=normal  \
		--borders=no --pro=color --columns 3 \
		--right-footer="" --left-footer=""    \
		--pretty-print=$R/etc/lua.ssh             \
		--footer="page %p."                     \
		-M letter -o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps; git add $@


