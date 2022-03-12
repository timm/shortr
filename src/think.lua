local _=require"tricks"
local class,cli,lines,o,oo=_.class,_.cli,_.lines,_.o,_.oo
local inc, inc3 , has, has3 = _.inc,_.inc3, _.has, _.has3
local THINK=class"THINK"

THINK.help=[[

lua think.lua [OPTIONS]

  -file -f data file = ../etc/data/breastcancer.csv
  -help -h help text                   = false
  -m    -m handle low frequency ranges = 2
  -k    -k handle low frequency classes = 1 
]]

local the = cli(THINK.help)

local h,e,names={}
for row in lines(the.file) do
  if not e then e={}; names=row else
    local h1 = row[#row]
    inc(h, h1)
    for c,x in pairs(row) do
      if x~="?" then 
        inc3(e, h1, c, x) end end end end 


for h1,n in pairs(h) do e1=e[h1]; print(h1, o(e1)) end
return THINK
