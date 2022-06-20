# vim:et:sw=2:ts=2:

MAKEFLAGS += --silent
SHELL:= /bin/bash#
R=$(shell git rev-parse --show-toplevel)

help : Makefile
	echo ""; printf "usage: make [OPTIONS]\n\n"
	@awk 'BEGIN {FS="[ \t]*:.*##[ \t]*"}  \
	  NF==2 { printf \
           "  \033[36m%-25s\033[0m %s\n","make " $$1,$$2}'  $< \
	| grep -v awk

src/*.lua: *.md
  gawk 'BEGIN {pre="-- "; } \
       /^```lua/     { print "-- " $0; next; pre="" } \
       /^```[ \t]*$/ { pre = "-- "  } \
                     { print pre $0 }' $^ > $@

ready:  ##  commit to main
	git add *;git commit -am save;git push;git status

docs/%.html: %.lua docs/%.md ## make html
	(sed 's/^/-- /' $(word 2,$^); awk 'BEGIN {FS="(-|[ \t]*)?->[ \t]*"}\
      NF==3 && /^-->/ { $$2=gensub(/([A-Za-z0-9_]+):/," `\\1`:  ","g",$$2);\
              print "--**"$$2"** <br> "$$3 ; next}\
      1' $(word 1,$^) ) > tmp.lua
	echo "docco: $< -> $@"
	docco -l classic  tmp.lua > /dev/null
	awk 'sub(/>tmp.lua</,">$<<") 1 ' docs/tmp.html > /tmp/$$$$; mv /tmp/$$$$  $@
	rm tmp.lua docs/tmp.html
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
