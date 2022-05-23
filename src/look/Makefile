looked.lua :  look.lua lib.lua looking.lua
	@rm -rf $@;
	@$(foreach d,$^,((figlet -W -fcybermedium $d; cat $d; echo "";)>>$@);)



../../docs/%.pdf : %.lua  
	@echo "pdf-ing $@ ... "
	@a2ps -BjR    -q                        \
		-L 120 \
		--line-numbers=1 -f 5                    \
		--highlight-level=normal  \
		--borders=no --pro=color --columns 2 \
		--right-footer="" --left-footer=""    \
		--pretty-print=../../etc/lua.ssh             \
		--footer="page %p."                     \
		-M letter -o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps; git add $@
