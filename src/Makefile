.PHONY: help tests hi bye pdfs

D      = ..
LUA    = $(shell ls *.lua)
PDF    = $(addsuffix .pdf, $(addprefix $D/docs/,$(basename $(LUA))))
HTML   = $(addsuffix .html,$(addprefix $D/docs/,$(basename $(LUA))))
COMMENT= gawk 'BEGIN {"-------------------------------------------------------------------"} \
                      /^[ \t]*$$/ {next} {print "---    "$$0} END{print}'

top:; echo $(LUA)

help:
	@printf "\nmake [OPTIONS]\n\nOPTIONS:\n"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {print(FS = ":.*?## "}; {printf "\033[36m%10s :\033[0m %s\n", $$1, $$2}'

h1:; @figlet -W -flarry3d     $t | $(COMMENT)
h3:; @figlet -W -fsmall  $${t%%.*}  | $(COMMENT)
h2:; @figlet -W -fcybermedium $t | $(COMMENT)

doc: $(PDF) $(HTML) $D/docs/index.html ## generate pdfs

tests: ## run tests
	ls $D/src/*.lua |entr -c etc/tasks.sh

hi: ## start work (update all files)
	git add *;git commit -am save;git push;git status

bye:  ## stop work (save all files)
	git add *;git commit -am save;git push;git status

#https://purecss.io/start/
$D/docs/index.html: $D/README.md $D/etc/head.html $D/etc/foot.html
	@echo "index-ing $@ ..."
	@(cat $D/etc/head.html; \
   python3 -m markdown -x toc -x extra $< | sed 's/docs\///g' ;\
   cat $D/etc/foot.html) > $@

$D/docs/%.md: %.lua ../README.md  Makefile
	@echo "md-ing $@..."
	@gawk 'BEGIN {FS="\n"; RS=""} {print $$0 "\n\n"; exit}' ../README.md > $@
	@sed '/^---/d' $< | sed '/^.---/d' | gawk -f ../etc/lua2md.awk  >>  $@
  
$D/docs/%.html: %.lua ../etc/dhead Makefile
	@echo "html-ing  $@ ..."
	@sed '/---/d' $< >> $D/docs/$<
	@docco -o $D/docs $D/docs/$< > /dev/null
	@gawk -i inplace '/<h1>/{while(getline line<"../etc/dhead")print line} 1' $@ 
	@rm -rf $D/docs/$<
	@cp $D/etc/docco.css $D/docs/

$D/docs/%.pdf : %.lua  
	@echo "pdf-ing $@ ... "
	@a2ps -BjR    -q                        \
		--line-numbers=1                    \
		--borders=no --pro=color --columns 2 \
		--right-footer="" --left-footer=""    \
		--pretty-print=$D/etc/lua.ssh             \
		--footer="page %p."                     \
		-M letter -o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps; git add $@

$D/docs/all.pdf:  all.lua

prune = grep -v "usr.bin.env" | grep -v "vi:"

# ../docs/all.lua: abcd.lua ako.lua all.lua bin.lua egs.lua \
#                  lib.lua num.lua rule.lua summary.lua sym.lua the.lua
INTROS=learn101.lua learn201.lua learn301.lua
DATA=ako.lua num.lua sym.lua summary.lua
EGS=egs.lua

pages:
	@(t="bnb" $(MAKE) h1; cat the.lua | $(prune)) >  all.lua
	@$(foreach d,bnb,(echo " "; cat $d | $(prune)) >>  all.lua;)
	@echo "" >> all.lua;$(foreach d,$(INTROS),(t="$d" $(MAKE) h3; cat $d | $(prune)) >>  all.lua;)
	@echo "" >> all.lua;$(foreach d,bin.lua rule.lua,(t="$d" $(MAKE) h3; cat $d | $(prune)) >>  all.lua;)
	@echo "" >> all.lua;$(foreach d,$(DATA),(t="$d" $(MAKE) h3; cat $d | $(prune)) >>  all.lua;)
	@echo "" >> all.lua;$(foreach d,$(EGS),(t="$d" $(MAKE) h3; cat $d | $(prune)) >>  all.lua;)
	@echo "" >> all.lua;$(foreach d, abcd.lua,(t="$d" $(MAKE) h3; cat $d | $(prune)) >>  all.lua;)
	@echo "" >> all.lua;$(foreach d,  lib.lua,(t="$d" $(MAKE) h3; cat $d | $(prune)) >>  all.lua;)

book: pages
	$(MAKE) ../docs/all.pdf
	open ../docs/all.pdf
	rm all.lua

bnb.lua: bnb
	cp $< $@

optA:
	lua brknbad.lua -f ../etc/data/auto93.csv -minItems .4 -t optimize2 | tee ~/tmp/dat

plotA: optA
	gnuplot ../etc/opt1.plt

BAD=./bnb
eg1 :; $(BAD) -t nb1
eg2 :; $(BAD) -t nb2
eg3 :; $(BAD) -g positive -f ../etc/data/diabetes.csv -t nb2  | cat -n
eg3a:; $(BAD) -g positive -f ../etc/data/diabetes.csv -t nb2a | cat -n
