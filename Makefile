MAKEFLAGS += --silent
SHELL:= /bin/bash#
R=$(shell git rev-parse --show-toplevel)
            #"  " say("make "$$1,31) " " say($$2,38) "  \033[36m%-25s\033[0m %s\n","make " $$1,$$2}'  $< \

help : Makefile
	@gawk 'function hue(x,c)   { return "\033[" c "m" x "\033[0m"  }       \
         BEGIN               { print(hue("\nusage: make[OPTIONS]\n",35)); \
                               FS="[ \t]*:.*##[ \t]*"}                     \
	       NF==2 &&/^[A-Za-z]/ { printf "  %-35s %s\n",hue("make "$$1,31),$$2}' $< 

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
	open $@
