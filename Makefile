.PHONY: help tests hi bye pdfs

LUA = $(shell ls *.lua)
PDF = $(addsuffix .pdf, $(addprefix docs/,$(basename $(LUA))))
HTML= $(addsuffix .html,$(addprefix docs/,$(basename $(LUA))))

help:
	@printf "\nmake [OPTIONS]\n\nOPTIONS:\n"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%10s :\033[0m %s\n", $$1, $$2}'

h1:
	@figlet -W -flarry3d $t | gawk '{print "-- " $$0}'

h2:
	@figlet -W -fogre $t | gawk '{print "-- " $$0}'

h3:
	@figlet -W -fcybermedium $t | gawk '{print "-- " $$0}'

doc: $(PDF) $(HTML) ## generate pdfs

tests: ## run tests
	ls *.lua |entr -c etc/tasks.sh

hi: ## start work (update all files)
	git add *;git commit -am save;git push;git status

bye:  ## stop work (save all files)
	git add *;git commit -am save;git push;git status

docs/%.html: %.lua
	docco -o docs $<
	cp etc/docco.css docs/

docs/%.pdf : %.lua  
	a2ps -BjR                            \
		--line-numbers=1                    \
		--borders=no --pro=color --columns 2 \
		--right-footer="" --left-footer=""    \
		--pretty-print=etc/lua.ssh             \
		--footer="page %p."                     \
		-M letter -o $@.ps $<
	ps2pdf $@.ps $@; rm $@.ps; git add $@
