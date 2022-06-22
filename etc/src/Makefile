MAKEFLAGS += --silent
SHELL:= /bin/bash#
R=$(shell git rev-parse --show-toplevel)

help : Makefile
	echo ""; printf "usage: make [OPTIONS]\n\n"
	@gawk 'BEGIN {FS="[ \t]*:.*##[ \t]*"}  \
	  NF==2 { printf \
           "  \033[36m%-25s\033[0m %s\n","make " $$1,$$2}'  $< \
	| grep -v awk

docs/index.html: docs/shortr.html ##  commit to main
	cp docs/shortr.html docs/index.html

ready:  ##  commit to main
	git add *;git commit -am save;git push;git status

docs/%.html: %.lua  README.md ## make html
	(cat README.md ; gawk '\
     BEGIN            { FS="(-|[ \t]*)?->[ \t]*"}\
     NF==3 && /^-->/ { $$2=gensub(/([A-Za-z0-9_]+):/," `\\1`:  ","g",$$2);\
                       print "--**"$$2"** <br> "$$3 ; next}\
      1' $< ) > tmp.lua
	echo "docco: $< -> $@"
	docco -l classic  tmp.lua > /dev/null
	gawk 'sub(/>tmp.lua</,">b(Ai)ttery ⤑ $<<") 1 ' docs/tmp.html > $@
	rm tmp.lua 
	cp $R/etc/docco.css docs/docco.css

docs/%.pdf : %.lua  ## make pdf
	@mkdir -p docs
	@echo "pdf-ing $@ ... "
	@a2ps -Bjr    -q                        \
		-L 125 \
		--line-numbers=1                    \
		--highlight-level=normal  \
		--borders=no --pro=color --columns 3 \
		--right-footer="" --left-footer=""    \
		--pretty-print=$R/etc/lua.ssh             \
		--footer="page %p."                     \
		-M letter -o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps; git add $@
