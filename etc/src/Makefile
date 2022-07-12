MAKEFLAGS += --silent
SHELL:= /bin/bash#
R=$(shell git rev-parse --show-toplevel)

WEB=$(HOME)/gits/baittery/baittery.github.io

help : Makefile
	echo ""; printf "usage: make [OPTIONS]\n\n"
	@gawk 'BEGIN {FS="[ \t]*:.*##[ \t]*"}  \
	  NF==2 { printf \
           "  \033[36m%-25s\033[0m %s\n","make " $$1,$$2}'  $< \
	| grep -v awk

loc:
	gawk '!/^(--|[ \t]*$$)/{n++} END {print n" lines"}' *.lua

docs/index.html: docs/all.html ##  commit to main
	cp docs/all.html docs/index.html

ready:  ##  commit to main
	git add *;git commit -am save;git push;git status

LUAS = $(wildcard [a-z]*.lua)

htmls:  $(subst .lua,.html,docs/$(subst lua ,html docs/,$(LUAS))) ## make all htmls
	cp -r docs/* $(WEB)/
	(cd $(WEB); git  add *; git commit -am save; git push)

docs/%.html: %.lua  README.md ## make html
	(cat README.md ; gawk '\
     BEGIN            { FS="(-|[ \t]*)?->[ \t]*"}\
     NF==3 && /^-->/ { $$2=gensub(/([A-Za-z0-9_]+):/," `\\1`:  ","g",$$2);\
                       print "--**"$$2"** <br> "$$3 ; next}\
      1' $< ) > tmp.lua
	echo "docco: $< -> $@"
	docco -l classic  tmp.lua > /dev/null
	gawk 'sub(/>tmp.lua</,">b(Ai)ttery ⤑ $(basename $<)<") 1 ' docs/tmp.html > $@
	rm tmp.lua 
	cp $R/etc/docco.css docs/docco.css

docs/%.md : %.lua  README.md
	echo $< 2>&1
	gawk -f 2md.awk $< > /tmp/page 2> /tmp/toc
	gawk 'sub(/TABLE.OF.CONTENTS/,"",$$0) {system("cat /tmp/toc")} 1' /tmp/page > $@

mds:   $(subst .lua,.md,docs/$(subst lua ,md docs/,$(LUAS))) ready ## make all htmls

xdocs/%.md: %.lua  README.md ## make mds
	echo $< 2>&1
	(cat README.md ; lua go.lua -f $< -g CHUNKS ) > $@

docs/_all.pdf:  ## make all pdf
	-rm tmp.lua
	$(foreach d,$(LUAS),(figlet -W -f larry3d $d; echo ""; cat $d; echo "")>>tmp.lua;)
	@mkdir -p docs
	@echo "pdf-ing $@ ... "
	@a2ps -KBjr             \
		-q                    \
		-L 125                 \
		--line-numbers=1        \
		--highlight-level=normal \
		--borders=no              \
		--pro=color                \
		--right-footer=""            \
		--left-footer=""              \
		--pretty-print=$R/etc/lua.ssh  \
		--footer="page %p."             \
		-M letter                        \
		--rows 1 --columns 3              \
		-o $@.ps tmp.lua
	rm tmp.lua
	@ps2pdf $@.ps $@; rm $@.ps; git add $@


docs/%.pdf : %.lua  ## make pdf
	@mkdir -p docs
	@echo "pdf-ing $@ ... "
	@a2ps -Bjr             \
		-q                    \
		-L 125                 \
		--line-numbers=1        \
		--highlight-level=normal \
		--borders=no              \
		--pro=color                \
		--columns 3                 \
		--right-footer=""            \
		--left-footer=""              \
		--pretty-print=$R/etc/lua.ssh  \
		--footer="page %p."             \
		-M letter                        \
		-o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps; git add $@
