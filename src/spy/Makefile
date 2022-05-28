MAKEFLAGS += --silent
SHELL:= /bin/bash#

help : Makefile
	echo ""; echo "usage: "
	@gawk 'BEGIN {FS="[ \t]*:.*##[ \t]*"}  \
	  NF==2 { printf \
           "  \033[36m%-15s\033[0m %s\n","make " $$1,$$2}'  $< \
	| grep -v gawk

c=spy
doc: ## make doc
	docco -l classic $c.lua	
