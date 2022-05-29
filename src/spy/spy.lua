 -- vim: ts=2 sw=2 et:
-- <img src=spy.jpg align=left width=250><hr>   
--      
-- <b>SEMI-SUPERVISED LANDSCAPE ANALYSIS</b>  
--     
-- [&copy; 2022](#copyright) Tim Menzies<br clear=all>
--   
-- |**classes:**   | [RANGE](#range) :: [SYM](#sym) :: [SOME](#some) :: [NUM](#num) :: [ROW](#row) :: [ROWS](#rows)|
-- |--------------:|-----------------------------------------------------------------------------------------------|   
-- |**functions:** | [Lib](#lib) :: [Demos](#demos) :: [Return](#return) :: [Start](#start)                        |
-- |**notes:**     | [Contribute](#contribute) :: [Copyright](#copyright)                                          |   
--      
-- &nbsp;   
--      
-- This code is an experiment in writing the _most_ learners in the _least_ code.
-- Here, each learner is just a few lines of code (since they are share the same
-- underlying code base). 
-- 
-- Given examples ((_x1,y1_), (_x2,y2_), ... etc, this
-- code explores things that make the most difference in the data.i
--  
-- -  Given _N_ examples,
-- we grabs, say,  **some=512** at random (to speed up the process) then find two extreme distant points
-- (to
-- avoid spurious outlier's, we pick a point that is only **far=95**%
-- most distant). 
-- - Distance is calculated using Minkowski (and at  **p=2**, this is 
-- a Euclidean measure) where numbers are normalized 0..1 (min..max) and symbols are at distance 0,1 
-- (for same,different). 
-- For missing values, we assume values that maximizes the distance.
-- - Using the _x_ values, 
-- we can position any other example _C &in; N_ somewhere between the two extremes.
-- If the extremes  _A,B_ are separated by
-- distance _c_,then _C_ has a distance _x=(a\*a + c\*c - b\*b)/(2c)_ from _A_. 
-- The data is divided
-- in two using the median _x_ value and we recurse (on all of _N_). 
-- - Optionally, we  might prune the half of the data nearest to
-- the worse extreme (where "worse" is defined by a multi-objective domination predicate 
-- (see [`ROW.__lt`](#rowlt). Note that this needs only two extreme points per pruning.
--    
-- When recursing, if one extreme point is taken from an extreme in the parent, then
-- (e.g.) 10,000 options can be pruned back to around 20
-- "good"  ones
-- in less than two  dozen evaluations. 
--  
-- The above heuristcs can be sometimes inaccurate,  so we loop a few times, each
-- time starting with some subset of "good" examples found in the last loop.
-- So of loop1 finds N<sup>**min1=.5**</sup> "good" examples, subsequent loops
-- look for **min2==10** best examples within  some of the "good" seen in prior
-- rounds.
--   
-- Once we have "good" and not "good" examples, the difference between them can summarise
-- what was found.  
-- - Those summaries are built from ranges that remember what `x` values
-- are seen in "good" and not "good". 
-- - For numbers, these ranges are built by dividing
-- lo to hi into 16 sized ranges (using (hi-lo)/**bins=16**). i
-- - This code combines ranges
-- if they have too few examples or if their "good"/not "good" distributions are uninformative
-- (see the  `div` in [NUM.bin](#numbin)). The resulting ranges are then ranked by
-- whatever is goal of the process (e.g. find choices that **goal=helps** us get to "good"). 
local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local help=[[  
SPY: semi-supervised landscape analysis    
(c) 2022 Tim Menzies, timm@ieee.org, BSD2 license    
"I think the highest and lowest points are the important    
 ones. Anything else is just... in between." ~Jim Morrison   
        
INSTALL: requires: lua 5.4+   
         download: spy.lua   
         test    : lua spy.lua -h   
         
USAGE: lua spy.lua [OPTIONS]   
                                              defaults   
                                              ~~~~~~~~   
  -F  --Far   normalized distance to extreme  = 95
  -S  --Seed  random number seed              = 10019   
  -G  --Goal  optimize for (helps,hurts,tabu) = helps   
  -b  --bins  number of bins                  = 16   
  -m  --min   min size pass1                  = .5   
  -p  --p     distance coefficient            = 2   
  -s  --some  sample size                     = 512   
          
OPTIONS (other):   
  -f  --file  csv file with data              = ../../etc/data/auto93.csv   
  -g  --go    start up action                 = nothing   
  -h  --help  show help                       = false]]   

-- ## Build `the` settings from comment string

-- At start-up, the first tasks is to create the settings object from the
-- top-of-file `help` string. 
-- - In that string, find lines starting with `--xx`.
-- - Add a slot to `the` with key `xx` and a value  
--   valie built from the last word on the line. 
-- - Check for updates (from command-line flags
--   `--xx` or `-x`).   
local the={}
local function toatom(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x  end

local function cli(key,x)
  x = tostring(x)
  for n,flag in ipairs(arg) do 
    if flag==("-"..key:sub(1,1)) or flag==("--"..key) then 
      x = x=="false" and"true" or x=="true" and"false" or arg[n+1] end end
  return toatom(x) end  
        
help:gsub(" [-][-]([^%s]+)[^\n]*%s([^%s]+)",function(k,x) the[k] = cli(k,x) end)

-- ## Define the local names

-- Define all local names at top of file (so code can be defined in any order).
local atom,big,bins,csv,fmt,goes,going,gt,is,lt,main,map
local o,oo,per,push,rand,rnd,sort,splice,tothing

-- Make polymorphic classes.   
-- - Define a `new` method that will add a link from a 
-- new table, back to a metatables.
-- -  Also, define a second link from that metatable
-- to a variable storing the methods. 
-- Further, add a print name and a print name for this class.
function is(name,    t,new)
  function new(kl,...) local x=setmetatable({},kl); kl.new(x,...); return x end 
  t = {__tostring=o, is=name}; t.__index=t 
  return setmetatable(t, {__call=new}) end 

-- ### Define the objects <a name=range></a>

-- - ROWS use ROWs (to store data) and NUMs or SYMs  to summarize the cells.
-- - ROWs  use ROWS to handle distance calculations and rankings.
-- - RANGEs remember the `ys` values seen between `xlo` and `xhi`.
--   - RANGEs use SYMs (to hold the `ys` counts)
--
-- NUMs use SOMEs which holds, at most `the.some` numbers
-- (and if it sees more than that, the SOMes just replaces 
--   an existing item, picked at random).
local NUM,  RANGE, ROW = is"NUM", is"RANGE", is"ROW" 
local ROWS, SOME,  SYM = is"ROWS", is"SOME", is"SYM" 
  
-- ## RANGE <a name=range></a>

-- - Remember the `ys` values seen between `xlo` and `xhi`.    
-- - Use SYMs (to remember the `ys` values).
-- - Score a range by (e.g.) how much it helps to 
--   select for a  `want'ed class (see **score**)
--   and for other goals, see **score.goal**).
-- - Report if a range is relevant to some row (see **selects**)   
--   
-- Implementation detail: ranges of SYMbolic columns have the
-- invariant `xlo==ylo` (while NUMeric ranges have xlo &le; xhi`).
function RANGE.new(i,at,txt,lo,hi,ys) 
  i.at,i.txt,i.xlo,i.xhi,i.ys=at,txt,lo,hi or lo,ys or SYM() end
   
function RANGE.add(i,x,y)
  if x<i.xlo then i.xlo = x end
  if x>i.xhi then i.xhi = x end
  i.ys:add(y) end
   
function RANGE.__tostring(i)
  local x, lo, hi = i.txt, i.xlo, i.xhi
  if     lo ==  hi  then return fmt("%s == %s",x, lo)  
  elseif hi ==  big then return fmt("%s >= %s",x, lo)  
  elseif lo == -big then return fmt("%s < %s", x, hi)  
  else                   return fmt("%s <= %s < %s",lo,x,hi) end end
    
function RANGE.score(i,want,B,R)
  local goal, b, r, z = {}, 0, 0, 1/big
  goal.helps= function(b,r) return ((b<r or b+r < .05) and 0) or b^2/(b+r) end
  goal.hurts= function(b,r) return ((r<b or b+r < .05) and 0) or r^2/(b+r) end
  goal.tabu= function(b,r) return 1/(b+r) end 
  for v,n in pairs(i.ys.all) do if v==want then b = b+n else r=r+n end end
  return goal[the.Goal](b/(B+z), r/(R+z)) end
   
function RANGE.selects(i,row,     v)
  v = row.cells[i.at]
  return v=="?" or (i.xlo==i.xhi and i.xlo==v) or (i.xlo<=v and v<i.xhi) end
-- ## SYM <a name=sym></a>

-- Update a summary of a  stream of symbols.
-- 
-- Report their mode (a.k.a. **mid()**) and -- entropy (a.k.a. **div()**).   
function SYM.new(i,at,txt) 
  i.at,i.txt = at or 0,txt or ""; i.n,i.all=0,{}; i.most,i.mode=0 end

-- Subtracting data is just the inverse of adding data.
function SYM.sub(i,x,inc) SYM.add(i,x,-(inc or 1)) end
-- Add by adjusting internal symbol counts.
function SYM.add(i,x,inc) 
  if x~="?" then 
    inc = inc or 1
    i.n = i.n+inc
    i.all[x] = inc + (i.all[x] or 0)
    if i.all[x] > i.most then i.most,i.mode=i.all[x], x end end end

-- Though NUMerics are discretized into (at most **the.bins**) ranges,
-- SYMs just get discretized to themelves.
function SYM.bin(i,x) return x end

-- While NUMeric ranges need some post-processing (after binning),
-- SYMbolic ranges are just ready to go.
function SYM.bins(i,bins,...) return bins end

function SYM.div(i,   e)
  e=0; for k,n in pairs(i.all) do if n>0 then e=e-n/i.n*math.log(n/i.n,2) end end 
  return e end

function SYM.mid(i,...) return i.mode end

---------------------------------------------------------------------------------
-- ## SOME <a name=some></a>

--  NUMs hold their summary
-- in a `SOME` object that, once it fills up, replaces old values with the new ones.
function SOME.new(i) i.all, i.ok, i.n = {}, false,0 end
function SOME.add(i,x,     a) 
  i.n, a = 1 + i.n, i.all
  if     #a     < the.some     then i.ok=false; push(a,x)  
  elseif rand() < the.some/i.n then i.ok=false; a[rand(#a)]=x end end 

function SOME.has(i) if not i.ok then sort(i.all) end;i.ok=true; return i.all end
---------------------------------------------------------------------------------
-- ## NUM <a name=num></a>

function NUM.new(i,at,txt) 
  i.at,i.txt=at or 0,txt or ""; i.hi= -big;i.lo= big; i.n,i.mu=0,0 
  i.w = i.txt:find"-$" and -1 or 1 
  i.all = SOME() end

function NUM.add(i,x) 
  if x ~="?" then 
    i.all:add(x) 
    i.n     = i.n + 1
    local d = x - i.mu
    i.mu    = i.mu + d/i.n
    i.hi=math.max(x, i.hi); i.lo=math.min(x, i.lo) end end

function NUM.bin(i,v,  b) 
  if i.hi - i.lo < 1E-9 then return 0 end
  b=(i.hi-i.lo)/the.bins;return math.floor(v/b+0.5)*b end
-- <a name=numbin></a>
function NUM.bins(i,bins,enough)
  print("enough", enough)
  local out={}
  local function cuts(lo,hi,       cut,lhs,rhs,all,tmp,n,best)
    lhs, rhs, all = SYM(), SYM(), SYM()
    for j=lo,hi do 
      for x,n in pairs(bins[j].ys.all) do all:add(x,n);rhs:add(x,n)end end
    n,best,cut = rhs.n, rhs:div()
    for j=lo,hi do
      for x,n in pairs(bins[j].ys.all) do lhs:add(x,n); rhs:sub(x,n) end
      if rhs.n >= enough and lhs.n >= enough then
        tmp = rhs:div()*rhs.n/n + lhs:div()*lhs.n/n 
        if tmp < best*1.01 then cut,best =j,tmp end end end
    if   cut 
    then cuts(lo, cut)
         cuts(cut+1, hi)
    else push(out, RANGE(i.at, i.txt, bins[lo].xlo, bins[hi].xhi, all)) end
  end --------------------
  cuts(1,#bins)
  for j=2,#out do out[j].xlo = out[j-1].xhi end
  out[1].xlo, out[#out].xhi = -big, big
  return out end

function NUM.clone(i)     return NUM(i.at,i.txt) end
function NUM.div(i,  a)   a=i.all:has(); return (per(a,.9) - per(a,.1))/2.56 end
function NUM.mid(i,p)     return rnd(i.mu,p or 3) end
function NUM.norm(i,x)
  return x=="?" and x or i.hi-i.lo<1E-9 and 0 or (x - i.lo)/(i.hi - i.lo) end
--------------------------------------------------------------------------------
-- ## ROW <a name=rowlt></a><a name=row></a>
-- 
function ROW.new(i,of,cells) i.of,i.cells,i.evaluated = of,cells,false end
function ROW.__lt(i,j,        n,s1,s2,v1,v2)
  i.evaluated = true
  j.evaluated = true
  s1, s2, n = 0, 0, #i.of.ys
  for _,col in pairs(i.of.ys) do
    v1,v2 = col:norm(i.cells[col.at]), col:norm(j.cells[col.at])
    s1    = s1 - 2.7183^(col.w * (v1 - v2) / n)
    s2    = s2 - 2.7183^(col.w * (v2 - v1) / n) end
  return s1/n < s2/n end
--------------------------------------------------------------------------------
-- ## ROWS <a name=rows></a>

function ROWS.new(i,src)
  i.all={}; i.cols={}; i.xs={}; i.ys={}; i.names={}
  if type(src)=="string" then for   row in csv(  src) do i:add(row) end 
                         else for _,row in pairs(src) do i:add(row) end end end

function ROWS.clone(i,inits,   j)
  j=ROWS{i.names}; for _,row in pairs(inits or {}) do j:add(row)end; return j end

function ROWS.add(i,row) 
  local function header(   col)
    i.names = row
    for at,s in pairs(row) do
      col = push(i.cols, (s:find"^[A-Z]" and NUM or SYM)(at,s))
      if not s:find":$" then
        if s:find"!$" then i.klass = col end
        push(s:find"[!+-]$" and i.ys or i.xs, col) end end 
  end -------------------------------
  if #i.cols==0 then header(row) else
    row = push(i.all, row.cells and row or ROW(i,row))
    for _,col in pairs(i.cols) do col:add(row.cells[col.at]) end end end

function ROWS.bestRest(i,  n,m)
  sort(i.all)
  n = #i.all
  m = n^the.min  
  return splice(i.all, 1,  m), splice(i.all, n - m) end

function ROWS.mid(i,    p,t) 
  t={}; for _,col in pairs(i.ys) do t[col.txt]=col:mid(p) end; return t end

function ROWS.bins(i,bests,rests)
  local function bins1(col,data,         tmp,bins,x,bin, out)
    tmp, bins = {}, {}
    for klass,rows in pairs{bests,rests} do
      for n,row in pairs(rows) do
        x = row.cells[col.at]
        if x ~= "?" then
          bin  = col:bin(x)
          tmp[bin] = tmp[bin] or push(bins, RANGE(col.at,col.txt,x))
          tmp[bin]:add(x,klass) end end end
    out = col:bins(sort(bins,lt"xlo"),  (#bests+#rests)^the.min)  
    if col.is=="NUM" then out[1].xlo , out[#out].xhi = -big, big end
    return #out >1 and out or {}
  end --------
  local out={}
  for _,col in pairs(i.xs) do 
     for _,b in pairs(bins1(col)) do 
       push(out,{range=b, score=b:score(1, #bests, #rests)}) end end
  return out end

function ROWS.contrast(i,bests,rests,hows,stop,n,how)
  stop = stop or #bests/4
  hows = hows or {}
  n    = n    or 1
  if (#bests + #rests) > stop then
    bins = i:bins(bests,rests)
    if #bins > 0 then
      bins = sort(bins,lt"score")
      map(bins, function(one) print("!!",one.range) end)
      how = bins[#bins].range
      print("best",o(how.ys.all))
      local bests1,rests1 = {},{}
      for _,t in pairs{bests,rests} do
        for _,r in pairs(t) do
          push(how:selects(r) and bests1 or rests1, r) end end
      if #bests1 < #bests or #rests1 < #rests then
        push(hows,how)
        return i:contrast(bests1, rests1, hows, stop, n+1) end end end
  return hows,bests end
--------------------------------------------------------------------------------
-- ## LIB <a name=lib></a>

big = math.huge
rand= math.random
fmt = string.format
     
function csv(csvfile) 
  csvfile = io.input(csvfile)
  return function(s, t) 
    s=io.read()
    if not s then io.close(csvfile) else
      t={}; for x in s:gmatch("([^,]+)") do t[1+#t] = toatom(x) end
      return t end end end 
   
function gt(x) return function(a,b) return a[x] > b[x] end end
function lt(x) return function(a,b) return a[x] < b[x] end end

local fails=0
function main(settings,funs,     defaults,todo)
  defaults = {}
  local function main1(fun,      status)
    for k,v in pairs(defaults) do settings[k]=v end 
    math.randomseed(settings.seed or 10019)
    io.stderr:write(".")
    status = fun() 
    if status ~= true then print("-- Error",one,status)
    fails = fails + 1 end 
  end -------------------       
  for k,v in pairs(settings) do defaults[k]=v end 
  todo = funs[settings.go] and {settings.go} or map(funs,
                 function(x) if type(funs[x])=="function" then return x end end)
  for _,one in pairs(sort(todo)) do main1(funs[one]) end end

function map(t,f,  u) u={}; for k,v in pairs(t) do u[1+#u]=f(v) end return u end
function oo(x)        print(o(x)); return x end
function o(t,    u)
  if #t>0 then return "{"..table.concat(map(t,tostring)," ").."}" end
  u={}; for k,v in pairs(t) do u[1+#u] = string.format(":%s %s",k,v) end
  return (t.is or "").."{"..table.concat(sort(u)," ").."}" end 
  
function per(t,p)    p=p*#t//1; return t[math.max(1,math.min(#t,p))] end
function push(t,x)   t[1+#t]=x; return x end
function rnd(n, p)   local m=10^(p or 0); return math.floor(n*m+0.5)/m  end
function sort(t,f)   table.sort(t,f); return t end
function splice( t, i, j, k,    u) 
  u={}; for n=(i or 1)//1, (j or #t)//1, (k or 1)//1 do u[1+#u]=t[n] end return u end
--------------------------------------------------------------------------------
-- ## Demos <a name=demos></a>

local no,go = {},{}

function go.the() oo(the); return true end

function go.ranges(       rows,bests,rests,ranges)
  rows = ROWS(the.file)
  bests,rests = rows:bestRest()
  ranges = rows:bins(bests,rests)
  for _,bin in pairs(ranges) do print(rnd(bin.score,3), bin.range) end
  return true,ranges end

function go.contrast(       rows,how, bests,rests,ranges,hows)
  rows = ROWS(the.file)
  bests,rests = rows:bestRest()
  hows,bests   = rows:contrast(bests,rests)
  print("all",#rows.all, o(rows:mid()))
  print("out",#bests,#hows,o(rows:clone(bests):mid()))
  for _,how in pairs(hows) do print(how) end
  return true end

--------------------------------------------------------------------------------
-- ## RETURN <a name=return></a>

if pcall(debug.getlocal, 4, 1) then return {
           o=o,oo=oo,the=the,EGS=EGS,NUM=NUM,RANGE=RANGE,
           ROW=ROW,ROWS=ROWS,SOME=SOME,SYM=SYM} end
--------------------------------------------------------------------------------
-- ## MAIN START UP <a name=start></a>


if the.help then os.exit(print(
  help:gsub("[%u][%u%d]+", "\27[31m%1\27[0m")           -- highlight capitals
      :gsub("\"[^\"]+\"", "\27[32m%1\27[0m")            -- highlight strings  
      :gsub("(%s)([-][-]?[^%s]+)(%s)","%1\27[33m%2\27[0m%3"),--highlight flags
  "")) end

main(the,go)

-- Last two lines. Do not change. Check for rogues and report any failures.
for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end--[5]
os.exit(fails)
--------------------------------------------------------------------------------
-- ## Notes
-- ### Contribute <a name=contribute></a>
-- If you offer updates to this code, please follow the following conventions.
--  
-- - Settings generated from "help" string
-- Settings can be updated from the strings seed in flags
-- Settings stored in the global "the"
-- - Layout lines 80 chars side (max,ish). Use 2 spaces for "tab".
-- Do functions as one-liners (if possible). Multi-line functions need a trailing
-- blank line. 
-- - Use `i` for self
-- - Use `col` for a col object.
-- - Use `r` for rows
-- - Use `v` for row cells values
-- -  Define all local names at top of file (so code can be defined in any order).
-- Otherwise, don't make much of use the "local" keyword (too ugly)
-- - Object names are short and UPPER CASE
-- Constructors need not return constructed instance.
-- No inheritance (hard to debug)
-- - Tests  in the "go" table at end. Reset settings to defaults after each
--   (see `goes`).
--   Tests pass if they return `true`, otherwose, add one to a `fails` counter.
-- - Command line "-go x" calls test "go.x()".
-- Command line "-h" shows help  
-- - 2nd last line: look for "rogue" globals (there should be none)
-- Last line: exit to operating system with number of failures seen in tests
--
-- ### Copyright <a name=copyright></a>
-- BSD 2-Clause License
--  
-- Copyright (c) [year], [fullname]
--  
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- 1. Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

