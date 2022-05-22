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
  --p     -p  distance coefficient        = 2
  --far   -f  far                         = .95 
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

-- i=self
-- v= cell value
-- c = column index
-- r= row (which is just a table)
-- s = string
-- t,u= table
local function num(s)    return s:find"^[A-Z]" end
local function goal(s)   return s:find"[!+-]$" end
local function weight(s) return s:find"-$" and -1 or 1 end 

local ROWS=is"ROWS"
function ROWS.new(i,src) 
  i.rows, i.nums, i.sx, i.ys, i.names =  {}, {},{},{}, nil
  if type(src)=="table" then for _,r in pairs(src) do i:add(r) end
                        else for r   in csv(src)   do i:add(r) end end end

function ROWS.add(i,r)
  if i.names then i:update(r); push(i.rows,row) else i:header(r) end 

function ROWS.header(i,r)
  i.names = r
  for c,s in pairs(r)do if num(s) then i.nums[c]={lo=big,hi=-big} end end 
  for c,s in pairs(r)do if goal(s)then i.ys[c]=weight(s) else i.xs[c]=c end end end

function ROWS.data(i,t,   v)
  for c,num in pairs(i.nums) do
    v = t[c]
    if v ~="?" then num.lo = math.min(v, num.lo)
                    num.hi = math.max(v, num.hi) end end end

function ROWS.norm(i,c,v,   lo,hi)
  lo,hi = i.nums[c].lo, i.nums[c].hi
  return (v=="?" and v) or ((hi-lo) < 1E-9 and 0) or (v-lo)/(hi-lo) end

function ROWS.dist(i,r1,r2,     d,n,dist1)
  function dist1(c,v1,v2)
    if v1=="?" and v2=="?" then return 0 end
    if not i.nums[c] 
    then return v1==v2 and 0 or 1 
    else if     v1=="?" then v2=norm(c,v2); v1= v2<.5 and 1 or 0 
         elseif v2=="?" then v1=norm(c,v1); v2= v1<.5 and 1 or 0 
         else   v1,v2 = norm(c,v1), norm(c,v2) end
         return math.abs(v1-v2) end 
  end ---------------------------
  d,n = 0,0
  for c,_ in pairs(i.xs) do n,d = n+1, d + (dist(c,r1[c], r2[c]))^the.p end
  return (d/n)^(1/the.p)  end

function ROWS.around(i,r1,t,          fun)
  function fun(r2) return {dist=dist(r1,r2), row=r2} end
  return sort(map(t or i.rows,fun),lt"dist") end

function ROWS.far(i,r1,t,   tmp)
  tmp= i:around(r1,t)
  return tmp[#tmp*the.far//1].row end

function ROWS.better(i,r1,r2,        n,s1,s2,v1,v2)
  n,s1,s2 = 0,0,0
  for _,__ in pairs(i.ys) do n = n + 1 end
  for c,w in pairs(i.ys) do
    v1,v2 = i:norm(c, r1[c]), i:norm(c, r2[c])
    s1    = s1 - 2.7183^(w * (v1 - v2) / n)
    s2    = s2 - 2.7183^(w * (v2 - v1) / n) end
  return s1/n < s2/n end

end
