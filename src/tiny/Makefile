R=$(shell git rev-parse --show-toplevel)

want = col/cols col/num col/sample col/sym    \
			 row/row row/rows \
			 lib/macros lib/maths lib/strings lib/lists \
			 lib/settings lib/structs lib/egs eg

all: 
	@figlet -W -f larry3d tiny.lua>_all.lua; cat tiny.lua>>_all.luat
	@$(foreach f,$(want),(figlet -W -f contessa $f>>_all.lua;cat $f.lua>>_all.lua;);)
	@$(MAKE) ../../docs/_all.pdf
	@rm _all.lua
	@open   ../../docs/_all.pdf
		
../../docs/%.pdf : %.lua
	mkdir -p $R/docs
	echo "pdf-ing $@ ... "
	@a2ps -B              \
		-q                   \
	  --chars-per-line=82   \
	  --file-align=fill      \
		--line-numbers=1        \
		--borders=no             \
		--pro=color               \
		--columns 3                \
		--right-footer="%Q"         \
		--left-footer="%W"           \
		--pretty-print=$R/etc/lua.ssh \
		--footer=""                    \
		-M letter                       \
		-o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps; git add $@
	@open $@
