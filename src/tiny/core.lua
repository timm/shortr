local help=[[  
TINY.LUA: landscape analysis 
(c) 2022 Tim Menzies, timm@ieee.org     
"I think the highest and lowest points are the important ones. 
  Anything else is just...in between." ~Jim Morrison

INSTALL:
  requires: lua 5.4+
  download: lib.lua, core.lua, cli.lua
  test    : lua egs.lua -h

USAGE:
  lua cli.lua [OPTIONS]

OPTIONS:                                  default
                                          -------
  --Also  -A  rest is 'also'*Best         = 3
  --Best  -B  use #t^Best as 'best'       = .5
  --bins  -b  max bins for numeric        = 16
  --Goal  -G  goal;  one of: up,down,over = up
  --keep  -k  max nums kept per column    = 256
  --seed  -s  random number seed          = 10019

OPTIONS (other):
  --file  -f  csv file with data          = ../etc/data/auto93.csv
  --help  -h  show help                   = false
  --loud  -l  show extra info             = false
  --go    -g  start up action             = nothing

Usage of the works is permitted provided that this instrument is
retained with the works, so that any entity that uses the works is
notified of this instrument. DISCLAIMER:THE WORKS ARE WITHOUT WARRANTY.]]

local _ = require"lib"
local big,push,tothing = _.big,_.push,_.tothing

local the={}
help:gsub(" [-][-]([^%s]+)[^\n]*%s([^%s]+)",function(k,x) the[k]=_.tothing(x)end)

function prune(src)
  local rows, nums = {},{}
  local function nump(s) return s:find"^[A-Z]" end
  local function goalp(s) return s:find"[!+-]$" end
  local function load(src,rows) 
    if type(src)=="table" then for _,row in pairs(src) do push(rows,row) end 
    else for row in csv(src) do push(rows,row) end end
    return rows end
  end ----------------------
  local function header(row)
    for c,s in pairs(rows[1]) do if nump(s) then nums[c]={lo=big,hi=-big} end end 
  end ------------------------
  local function data(row,   x)
    for c,num in pairs(nums) do
      x=row[c]
      if x ~="?" then num.lo = math.min(x,num.lo)
                      num.hi = math.max(x,num.hi) end end 
  end ---------------------
  rows = load(src,{})
  for n,row in pairs(rows) do
    if n==1 then  header(row) else update(row); push(rows,row) end end end
end
