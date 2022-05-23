looked.lua :  look.lua lib.lua looking.lua
	@rm -rf $@;
	@$(foreach d,$^,((figlet -W -fcybermedium $d; cat $d; echo "";)>>$@);)



../../docs/%.pdf : %.lua  
	@echo "pdf-ing $@ ... "
	@a2ps -BjR    -q                        \
		-l 82 \
		--line-numbers=1                     \
		--highlight-level=normal  \
		--borders=no --pro=color --columns 3 \
		--right-footer="" --left-footer=""    \
		--pretty-print=../../etc/lua.ssh             \
		--footer="page %p."                    \
		-M letter -o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps; git add $@


V=../../etc/data
VS=$V/auto2.csv $V/SSN.csv $V/china.csv $V/coc1000.csv $V/healthCloseIsses12mths0001-hard.csv $V/healthCloseIsses12mths0011-easy.csv $V/nasa93dem.csv $V/pom.csv

all: 
	@$(foreach  v, $(VS), echo $v; lua looking.lua -f $v -g look;)

