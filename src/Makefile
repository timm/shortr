.PHONY: help tests hi bye pdfs

D=..
LUA = $(shell ls *.lua)
PDF = $(addsuffix .pdf, $(addprefix $D/docs/,$(basename $(LUA))))
HTML= $(addsuffix .html,$(addprefix $D/docs/,$(basename $(LUA))))
COMMENT=gawk '/^[ \t]*$$/ {next} {print "--     "$$0}'

top:; echo $(LUA)

help:
	@printf "\nmake [OPTIONS]\n\nOPTIONS:\n"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%10s :\033[0m %s\n", $$1, $$2}'

h1:; @figlet -W -flarry3d     $t | $(COMMENT)
h2:; @figlet -W -fogre        $t | $(COMEMNT)
h3:: @figlet -W -fcybermedium $t | $(COMMENT)

doc: $(PDF) $(HTML) $D/docs/index.html ## generate pdfs

tests: ## run tests
	ls $D/src/*.lua |entr -c etc/tasks.sh

hi: ## start work (update all files)
	git add *;git commit -am save;git push;git status

bye:  ## stop work (save all files)
	git add *;git commit -am save;git push;git status

#https://purecss.io/start/
$D/docs/index.html: $D/README.md $D/etc/head.html $D/etc/foot.html
	(cat $D/etc/head.html; \
   python3 -m markdown -x toc -x extra $< | sed 's/docs\///g' ;\
   cat $D/etc/foot.html) > $@

$D/docs/%.html: %.lua
	docco -o $D/docs $<
	cp $D/etc/docco.css $D/docs/

$D/docs/%.pdf : %.lua  
	a2ps -BjR                            \
		--line-numbers=1                    \
		--borders=no --pro=color --columns 2 \
		--right-footer="" --left-footer=""    \
		--pretty-print=$D/etc/lua.ssh             \
		--footer="page %p."                     \
		-M letter -o $@.ps $<
	ps2pdf $@.ps $@; rm $@.ps; git add $@
