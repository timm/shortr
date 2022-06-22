-- ## Options
       
local m=require"lib"
m.the = m.cli( m.opts( [[
  
TRICKS : bag of LUA tricks, (c) 2022 Tim Menzies, BSD2
  
OPTIONS:
  -s  --seed    random number seed = 10019
  
OPTIONS (other):
  -f  --file    data file        = ../../data/auto93.csv
  -g  --go      start up action  =  nothing,
  -h  --help    show help        =  false
]]))
return m
