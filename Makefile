MAKEFLAGS += --silent
SHELL:= /bin/bash#
R=$(shell git rev-parse --show-toplevel)

help : Makefile
	echo ""; printf "usage: make [OPTIONS]\n\n"
	@gawk 'BEGIN {FS="[ \t]*:.*##[ \t]*"}  \
	  NF==2 { printf \
           "  \033[36m%-25s\033[0m %s\n","make " $$1,$$2}'  $< \
	| grep -v gawk

hi: docs/index.html ##  update from  main
	git pull

m?="save"
bye: docs/index.html ##  commit to main  
	git add *;git commit -am "$m" ;git push;git status

docs/index.html: docs/l5.html ## make home page
	cp $< $@

docs/%.html:  %.lua ## make doc
	mkdir -p docs 
	docco -l classic $<
	cp $R/etc/docco.css docs/

docs/%.pdf : %.lua  ## make pdf
	mkdir -p docs 
	echo "pdf-ing $@ ... "
	a2ps -Bjr    -q                        \
		-L 125 \
		--line-numbers=1                    \
		--highlight-level=normal  \
		--borders=no --pro=color --columns 3 \
		--right-footer="" --left-footer=""    \
		--pretty-print=$R/etc/lua.ssh             \
		--footer="page %p."                     \
		-M letter -o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps; git add $@

c= SSM auto2 auto93 china coc1000 coc10000 \
   healthCloseIsses12mths0001-hard healthCloseIsses12mths0011-easy \
   nasa93dem pom

contrasts: 
	$(foreach d,$c, (echo "";echo $d;lua l5.lua -f data/$d.csv -g contrast);)
