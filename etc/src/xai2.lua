-- For simple XAI (explainable AI), try a little sampling theory and a
-- little learning.
--
-- For example, if we apply a sorting heuristic to data, we can binary
-- chop our way down to good solutions. Assuming such chops, 
--  at probability _P_, we find _q_
-- percent "best" items (where "best" is
-- defined by the Zitzler's multi-objective indicator) using
-- `n=log2(log(1-P)/log(1-q))` samples. e.g. the 5% best within 10,000 samples
-- is hunted down using less than n=10 samples.  Sounds too good to be true?
-- Well lets check.
--     
-- This code starts with a config variable (`the`)
-- and ends with a library of demos (see the `go` functions at end of file).
-- Each setting can be (optionally) updated by a command-line flag.
-- Demos can be run separately or  all at once (using `-g all`).
-- For regression tests, we report the failures seen when the demos run.
--    
-- <img src="xai4.jpeg" width=200 align=left>
--    
-- This code makes extensive use of a DATA object.  Data from disk
-- becomes a DATA. DATA  are recursive bi-clustered by partitioning on
-- the distance to two distant ROWs (found via the FASTMAP
-- linear time random
-- projection algorithm).  Each cluster is new DATA object, containing a subset
-- of the data. A decision tree is built that reports the difference
-- between the "best" and "worst" clusters (defined using a multi-objective
-- domination predicate) and that tree is just a  tree
-- of DATAs with `kids` pointer to sub-DATAs).  This process
-- only needs log2(N) queries to y-values (while clustering,
-- just on the pairs of
-- distance objects).
-- 
-- convenntions "is" [refix is a bookeam. "n" is a number, sprefix=string
-- _ prefix means internal function
local the= {
     about ={ what = "XAI.LUA",
              why  = "Multi-objective semi-supervised explanation",
              who  = "Tim Menzies <timm@ieee.org>",
              when = 2022,
              copyright = "BSD-2 clause license",
              how  = "USAGE: lua xai.lua -[bFfgmnpsS] [arg]"},
     Balance= 4,        -- for delta, ratio rest:best 
     bins   = 16,       -- for bins, initial #bins  (before merging)
     Far    = .95,      -- for far, how far to look for distant pole
     files  = "../../data/auto93.csv",
     go     = "nothing", -- start up action
     min    = .5,        -- for bestOrRest, cluster down to N^min groupings
     ratios = 512,       -- for RATIO, max sample size
     p      = 2,         -- for dist, distance coeffecient 
     seed   = 10019,     -- random number seed
     Some   = 512,       -- for far, how many rows to explore 
     stop   = 6          -- for delta, min row size.
     }
---- ---- ---- ---- Names
---- Cache names known `b4` we start
-- Use that, later, to hunt down any rogue globals.
local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end
local function rogues()
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end end
---- Types
local ABOUT,COL,DATA,NOM,RATIO,ROW,XY,
---- learning functions
     add,add2,adds,around,bestOrRest, bestxy,bestxys,
     bins,clone,half,has,
     better, csv2data,dist,div,
     half, is, key,many,mid,norm,
     xyShow, row, selects, stats,
---- General functions
     any,big,cat,chat,cli,coerce,csv,fmt,get,gt,
     lines,lt,map,obj,per,push,
     rand,rev,rnd,same,shuffle, slice,sort,values,words
---- Startup
local go={}

---- This module
local XAI= {-- to be completed later
      the=the, rogues=rogues, go=go,
      ABOUT=ABOUT,COL=COL,DATA=DATA,NOM=NOM,RATIO=RATIO,ROW=ROW,XY=XY}

function obj(sName,     new,self,t)
 function new(k,...)
   self = setmetatable({},k)
   return setmetatable(k.new(self,...) or self,k) 
  end ----------------- 
  t={_is=sName, __tostring=function(x) return cat(x) end}
  t.__index = t
  return setmetatable(t,{__call=new}) end

local ABOUT,DATA,NOM=obj"ABOUT", obj"DATA", obj"NOM"
local RATIO,ROW,XY=obj"RATIO",obj"ROW",obj"XY"
---- ---- ---- ---- Types
-- In this code,  function arguments offer some type hints. 
-- `xs` denotes a list of type `x` for
-- x in bool, str, num, int or one of the user defined types.
-- `t` denotes a list of any type. User-defined types are create by functions
-- with UPPER CASE names. Any argument with spaces before it is optional.
-- Any arguments with more than two spaces before it are local vals (so don't use those).

-- **`is` recognizes column types.**  
-- These column types appear in first row of our  CSV files.
is={num   = "^[A-Z]",  -- ratio cols start with uppercase
    goal  = "[!+-]$",  -- !=klass, [+,-]=maximize,minimize
    klass = "!$",      -- klass if "!"
    skip  = ":$",      -- skip if ":"
    less  = "-$"}      -- minimize if "-"

local function col(sName,iAt)
  sName = sName or ""
  return {n    = 0,                -- how many items seen?
          at   = iAt or 0,         -- position ot column
          txt  = sName,            -- column header
          w    = sName:find(is.less) and -1 or 1,
          ok   = true,             -- false if some update needed
          has  = {}} end           -- place to keep (some) column values.

-- **RATIO are special COLs that handle ratios.**      
-- **NOM are special COLs that handle nominals.**
function RATIO:new(  sName,iAt) return col(sName,iAt) end

function NOM:new(  sName,iAt) return col(sName,iAt) end

-- **ROW holds one record of data.**
function ROW:new(about,t)
  return {
          _about=about,       -- pointer to background column info
          cells=t,            -- raw values
          cooked=nil,         -- for (e.g) discretized values
          rank=0,             -- position between 1..100
          evaled=false} end   -- true if we touched the y-values

-- **DATA holds many `ROWs`**   
--  whose values are summarized in `ABOUT`.
function DATA:new() return {rows={}, about=nil} end

-- **ABOUT is a factory for making columns from column header strings.**  
-- Goals and none-gaols are cached in `x` and `y` (ignorong
-- anything that is `skipped`.
function ABOUT:new(sNames)
  local about = {names=sNames,all={}, x={}, y={}, klass=nil}
  for at,name in pairs(sNames) do
    local one = name:find(is.num) and RATIO(name,at) or NOM(name,at)
    push(about.all, one)
    if not name:find(is.skip) then
      push(name:find(is.goal) and about.y or about.x, one)
      if name:find(is.klass) then about.klass=one end end end
  return about end

-- **XY summarize data from the same rows from two columns.**   
-- `num2` is optional (defaults to `num1`).   
-- `y` is optional (defaults to a new NOM)
function XY:new(txt,at,num1,num2,nom)
  return {txt = txt,
          at  = at,
          xlo = num1, 
          xhi = num2 or num1, 
          y   = nom or NOM(txt,at)} end


---- ---- ---- ---- Functions for Types
---- ---- ---- Create
-- **Copy the structure of `data`.**    
-- Optionally, add rows of data (from `t`).
function DATA:clone(t)
  local data1= DATA()
  data1:add(self.about.names)
  for _,row1 in pairs(t or {}) do data1:add(row1) end
  return data1 end

---- ---- ---- Update
-- **Add something into one `col`.**  
-- For `NOM` cols, keep a count
-- of how many times we have seen `x'. For RATIO columns,
-- keep at most `the.ratios` (after which, replace old items at random).   
-- `inc` is optional (it is  little hack used during 
--  discretization for very
-- for fast NOM merging).
function NOM:add(x,  num)
  if x ~= "?" then
    num = num or 1
    self.n = self.n + num
    self.has[x] = num + (self.has[x] or 0) end end

function RATIO:add(x)
  if x ~= "?" then
    local pos
    self.n = self.n + 1
    if     #self.has < the.ratios        then pos = 1 + (#self.has) 
    elseif rand()    < the.ratios/self.n then pos = rand(#self.has) end
    if pos then
      self.ok=false -- the `kept` list is no longer in sorted order
      self.has[pos]=x end end end

-- **Add in `x,y` values from one row into an XY.**
function XY:add(x,y)
  self.xlo = math.min(x, self.xlo)
  self.xhi = math.max(x, self.xhi)
  self.y:add(y) end

-- **Add a row of values, across all columns.**    
-- This code implements _row sharing_; i.e. once a row is created,
-- it is shared across many DATAs. This means that (e.g.) distance 
-- calcs are normalized across the whole space and not specific sub-spaces.
-- To disable that, change line one of this function to   
-- `local row = ROW(about,x.cells and x.cells or x)` 
function ABOUT:add(t)
  local row = t.cells and t or ROW(self,t) -- ensure that "x" is a row.
  for _,cols in pairs{self.x,self.y} do
    for _,col in pairs(cols) do col:add(row.cells[col.at]) end end
  return row end

-- **Add a `row` to `data`.**   
-- If this is top row, use `t` to initial `data.about`.
function DATA:add(t)
  if   self.about  -- not first row
  then push(self.rows, self.about:add(t))
  else self.about = ABOUT(t) end end

---- ---- ---- Print
-- **Print one xy**.
function XY:__tostring()
  local x,lo,hi = self.txt, self.xlo, self.xhi
  if     lo ==  hi  then return fmt("%s == %s", x, lo)
  elseif hi ==  big then return fmt("%s >  %s", x, lo)
  elseif lo == -big then return fmt("%s <= %s", x, hi)
  else                   return fmt("%s <  %s <= %s", lo,x,hi) end end

---- ---- ---- Query
-- **Return `col.has`, sorting numerics (if needed).**
function NOM:holds() return self.has end
function RATIO:holds()
  if not self.ok then table.sort(self.has) end
  self.ok=true 
  return self.has end

-- **Return `num`, normalized to 0..1 for min..max.**
function RATIO:norm(num)
  local a= self:holds() -- "a" contains all our numbers,  sorted.
  return a[#a] - a[1] < 1E-9 and 0 or (num-a[1])/(a[#a]-a[1]) end

-- **Return the central tendency of `col`umns**  
--  (median/mode for ratios/nominals (respectively).
function NOM:mid(places)
  local mode,most= nil,-1
  for x,n in pairs(self.has) do if n > most then mode,most=x,n end end
  return mode end

function RATIO:mid(places)
  local median= per(self:holds(),.5)
  return places and rnd(median,places) or median end 

-- **Return the `div`ersity of a `col`umns**   
-- (sd/entropy for ratios/nominals (respectively).
function RATIO:div(places)
  local nums=self:holds()
  local out = (per(nums,.9) - per(nums,.1))/2.58 
  return places and rnd(out,places) or out end 

function NOM:div(places)
  local out = 0
  for _,n in pairs(self.has) do
    if n>0 then out=out-n/self.n*math.log(n/self.n,2) end end 
  return places and rnd(out,places) or out end 

-- **Returns stats collected across a set of `col`umns**   
function DATA:mid(places,cols,    u)
  u={}; for k,col in pairs(cols or self.about.y) do 
          u.n=col.n; u[col.txt]=col:mid(places) end
  return u end

function DATA:div(places,cols,    u)
  u={}; for k,col in pairs(cols or self.about.y) do 
          u.n=col.n; u[col.txt]=col:div(places) end
  return u end

-- **Return true if `row1`'s goals are better than `row2`.**
function better(row1,row2)
  row1.evaled,row2.evaled= true,true
  local s1,s2,d,n,x,y=0,0,0,0
  local ys,e = row1._about.y,math.exp(1)
  for _,col in pairs(ys) do
    x,y= row1.cells[col.at], row2.cells[col.at]
    x,y= col:norm(x), col:norm(y)
    s1 = s1 - e^(col.w * (x-y)/#ys)
    s2 = s2 - e^(col.w * (y-x)/#ys) end
  return s1/#ys < s2/#ys end

---- ---- ---- Dist
-- Return 0..1 for distance between two rows using `cols`
-- (and `cols`` defaults to the `x` columns).
function dist(row1,row2,cols)
  local d,n,x,y,dist1=0,0
  function dist1(col,x,y)
    if x=="?" and y=="?" then return 1 end
    if   col.isNom
    then return (x=="?" or y=="?") and 1 or x==y and 0 or 1 
    else if     x=="?" then y=norm(col,y); x=y<.5 and 1 or 0
         elseif y=="?" then x=norm(col,x); y=x<.5 and 1 or 0
         else   x,y = norm(col,x), norm(col,y) end
         return math.abs(x-y) end
  end ---------------
  cols = cols or row1._about.x
  for _,col in pairs(cols) do
    x,y = row1.cells[col.at], row2.cells[col.at]
    d   = d + dist1(col,x,y)^the.p
    n   = n + 1 end
  return (d/n)^(1/the.p) end

-- Return all rows  sorted by their distance  to `row`.
function around(row1,rows)
  return sort(map(rows, function(row2) return {row=row2,d=dist(row1,row2)} end),
             lt"d") end

---- ---- ---- Clustering
-- **Divide data according to its distance to two distant rows.**   
-- Use all the `best` and some sample of the `rest`.
local half={}
function half.splits(rows)
  local best,rest0 = half._splits(rows)
  print("!",cat(sort(map(rows,function(row) if row.evaled then return row.rank end end))))
  local rest = many(rest0, #best*the.Balance)
  local both = {}
  for _,row in pairs(rest) do push(both,row).label="rest" end
  for _,row in pairs(best) do push(both,row).label="best" end
  return best,rest,both end

-- Divide the data, recursing into the best half. Keep the
-- _first_ non-best half (as _worst_). Return the
-- final best and the first worst (so the best best and the worst
-- worst).
function half._splits(rows,  rowAbove,          stop,worst)
  stop = stop or (#rows)^the.min
  if   #rows < stop
  then return rows,worst or {} -- rows is shriving best
  else local A,B,As,Bs = half._split(rows,rowAbove)
       if   better(A,B)
       then return half._splits(As,A,stop,worst or Bs)
       else return half._splits(Bs,B,stop,worst or As) end end end

-- Do one split. To reduce the cost of this search,
-- only apply it to `some` of the rows (controlled by `the.Some`).
-- If `rowAbove` is supplied,
-- then use that for one of the two distant items (so top-level split seeks
-- two poles and lower-level poles only seeks one new pole each time).
function half._split(rows,  rowAbove)
  local As,Bs,A,B,c,far,project = {},{}
  local some= many(rows,the.Some)
  function far(row) return per(around(row,some), the.Far).row end
  function project(row) 
    return {row=row, x=(dist(row,A)^2 + c^2 - dist(row,B)^2)/(2*c)} end
  A= rowAbove or far(any(some))
  B= far(A)
  c= dist(A,B)
  for n,rd in pairs(sort(map(rows, project),lt"x")) do
    push(n < #rows/2 and As or Bs, rd.row) end
  return A,B,As,Bs,c end

---- ---- ---- Discretization
-- **Divide column values into many bins, then merge unneeded ones**   
-- When reading this code, remember that NOMinals can't get rounded or merged
-- (only RATIOS).
local bins={}
function bins.find(rows,col)
  local n,xys = 0,{} 
  for _,row in pairs(rows) do
    local x = row.cells[col.at]
    if x~= "?" then
      n = n+1
      local bin = col.isNom and x or bins._bin(col,x)
      local xy  = xys[bin] or XY(col.txt,col.at, x)
      add2(xy, x, row.label)
      xys[bin] = xy end end
  xys = sort(xys, lt"xlo")
  return col.isNom and xys or bins._merges(xys,n^the.min) end

-- RATIOs get rounded into  `the.bins` divisions.
function bins._bin(ratio,x,     a,b,lo,hi)
  a = ratio:holds()
  lo,hi = a[1], a[#a]
  b = (hi - lo)/the.bins
  return hi==lo and 1 or math.floor(x/b+.5)*b  end 

-- While adjacent things can be merged, keep merging.
-- Then make sure the bins to cover &pm; &infin;.
function bins._merges(xys0,nMin) 
  local n,xys1 = 1,{}
  while n <= #xys0 do
    local xymerged = n<#xys0 and bins._merged(xys0[n], xys0[n+1],nMin) 
    xys1[#xys1+1]  = xymerged or xys0[n]
    n = n + (xymerged and 2 or 1) -- if merged, skip next bin
  end
  if   #xys1 < #xys0 
  then return bins._merges(xys1,nMin) 
  else xys1[1].xlo = -big
       for n=2,#xys1 do xys1[n].xlo = xys1[n-1].xhi end 
       xys1[#xys1].xhi = big
       return xys1 end end

-- Merge two bins if they are too small or too complex.
-- E.g. if each bin only has "rest" values, then combine them.
-- Returns nil otherwise (which is used to signal "no merge possible").
function bins._merged(xy1,xy2,nMin)   
  local i,j= xy1.y, xy2.y
  local k = NOM(i.txt, i.at)
  for x,n in pairs(i.has) do add(k,x,n) end
  for x,n in pairs(j.has) do add(k,x,n) end
  local tooSmall   = i.n < nMin or j.n < nMin 
  local tooComplex = div(k) <= (i.n*div(i) + j.n*div(j))/k.n 
  if tooSmall or tooComplex then 
    return XY(xy1.txt,xy1.at, xy1.xlo, xy2.xhi, k) end end 

---- ---- ---- Rules
-- **Find the xy range that most separates best from rest**      
-- Then call yourself recursively on the rows selected by the that range.   
local how={}
function how.rules(data) return how._rules1(data, data.rows) end

function how._rules1(data,rowsAll, nStop,xys)
  xys = xys or {}
  nStop = nStop or the.stop
  if #data.rows > nStop then 
    local xy = how._xyBest(data)
    if xy then 
      local rows1 = how._selects(xy, data.rows)
      if rows1 then
        push(xys,xy)
        print(cat(how._evals(rowsAll)),
				  xyShow(xy), how._nevaled(rowsAll),#rows1)
        return how._rules1(clone(data,rows1),rowsAll, nStop,xys) end end  end
  return xys,data end 

-- Return best xy across all columns and ranges.
function how._xyBest(data)
  local best,rest,both = half.splits(data.rows)
  local most,xyOut = 0
  for _,col in pairs(data.about.x) do
    local xys = bins.find(both,col)
    if #xys > 1 then
      for _,xy in pairs(xys) do
        local tmp= how._score(xy.y, "best", #best, #rest)
        if tmp > most then most,xyOut = tmp,xy end end end end 
  return xyOut end 

function how._nevaled(rows,     n)
  n=0;for _,row in pairs(rows) do if row.evaled then n=n+1 end end;return n end

function how._evals(rows,     n)
  return sort(map(rows,function(row) if row.evaled then return row.rank end end)) end

-- Scores are greater when a NOM contains more of the `sGoal` than otherwise.
function how._score(nom,sGoal,nBest,nRest)
  local best,rest=0,0
  for x,n in pairs(nom.has) do
    if x==sGoal then best=best+n/nBest else rest=rest+n/nRest end end
  return  (best - rest) < 1E-3 and 0 or best^2/(best + rest) end

-- Returns the subset of rows relevant to an xy (and if the subset 
-- same as `rows`, then return nil since they rule is silly).
function how._selects(xy,rows)
  local rowsOut={}
  for _,row in pairs(rows) do
    local x= row.cells[xy.at]
    if x=="?" or xy.xlo==xy.xhi and x==xy.xlo or xy.xlo<x and x <=xy.xhi then 
      push(rowsOut,row) end end 
  if #rowsOut < #rows then return rowsOut end end

---- ---- ---- ---- General Functions
---- ---- ---- Misc
function same(x) return x end

---- ---- ---- Maths
-- Large number
big=math.huge
-- Random num
rand=math.random

-- Round nums.
function rnd(num, places)
  local mult = 10^(places or 3)
  return math.floor(num * mult + 0.5) / mult end

---- ---- ---- Lists
-- Return any item (selected at random) from list `t`.
function any(t) return t[rand(#t)] end

-- Return `num` items (selected at random) from list `t`.
-- If `num` is more than the size of the list, return that list, shuffled.
function many(t,num, u)
  if num>#t then return shuffle(t) end
  u={}; for j=1,num do u[1+#u]= any(t) end; return u end

-- Return items in `t` filtered through `f`. If `f` ever returns nil
-- then the returned list will be shorter.
function map(t,f)
  local u={}; for _,v in pairs(t) do u[1+#u]=f(v) end; return u end

-- Helper function for `map` (extracts certain slots
function get(x) return function(t) return t[x] end end

-- Return the `p`-th item in `t` (assumed to be sorted). e.g.
-- `per(t,.5)` returns the median.
function per(t,p)
  p=math.floor((p*#t)+.5); return t[math.max(1,math.min(#t,p))] end

-- Add `x` to list `t`, returning `x`.
function push(t,x) t[1+#t]=x; return x end

-- In-place reverse, return reversed list
function rev(t)
  for i=1, math.floor(#t / 2) do t[i],t[#t-i+1] = t[#t-i+1],t[i] end
  return t end

-- Randomly shuffle, in place, the list `t`.
function shuffle(t,   j)
  for i=#t,2,-1 do j=rand(i); t[i],t[j]=t[j],t[i] end; return t end

-- Return `t` from `go` to `stop` by `inc`.     
-- `go` is optional (defaults to 1).
-- `stop` is optional (defaults to length of `t`).       
-- `inc` is optional (defaults to 1)
function slice(t, go, stop, inc)
  local u={}
  for j=(go or 1)//1,(stop or #t)//1,(inc or 1)//1 do u[1+#u]=t[j] end
  return u end

-- In-place sort, returns sorted list
function sort(t,f) if #t==0 then t=values(t) end; table.sort(t,f); return t end

-- Sorting predictates
function lt(x) return function(a,b) return a[x] < b[x] end end
function gt(x) return function(a,b) return a[x] > b[x] end end

-- Return values in a table
function values(t,   u) u={}; for _,v in pairs(t) do u[1+#u]=v end; return u end

---- ---- ---- Print
-- Generate a string from `t`.
function cat(t,   seen,      show,u,pub)
  if type(t)~="table" then return tostring(t) end
  seen = seen or {}
  if seen[t] then return "..." end
  seen[t]=t
  function show(k,v)
    if tostring(k):sub(1,1) ~= "_" then
      v=cat(v,seen)
      return #t==0 and fmt(":%s %s",k,v) or tostring(v) end end
  u={}; for k,v in pairs(t) do u[1+#u]=show(k,v) end
  return (t._is or "").."{"..table.concat(#t==0 and sort(u) or u," ").."}" end

-- Generate a string from `t` and print it (returning `t`).
function chat(t) print(cat(t)) return t end

-- Emulate Printf
fmt = string.format

---- ---- ---- Read
-- Try reading `str` as a boolean, then int, then float, then string.
function coerce(str)
  str = str:match"^%s*(.-)%s*$"
  if str=="true" then return true elseif str=="false" then return false
  else return math.tointeger(str) or tonumber(str) or str end  end

-- Read update for `slot` of table from command line flag `-s` or `--slot`.
-- If slot's is a boolean, this code flips old value.
function cli(t)
  for slot,v in pairs(t) do
    v = tostring(v)
    for n,x in ipairs(arg) do
      if x=="-"..(slot:sub(1,1)) or x=="--"..slot then
        v = v=="false" and "true" or v=="true" and "false" or arg[n+1] end end
    t[slot] =  coerce(v) end
  return t end

-- Split  `str` on `sep`, filter each part through `fun`, return the resulting list.
function words(str,sep,fun,      t)
  fun = fun or function(z) return z end
  sep = fmt("([^%s]+)",sep)
  t={};for x in str:gmatch(sep) do t[1+#t]=fun(x) end;return t end

-- Read lines from `filestr`, closing stream at end. Call `fun` on each line.
function lines(filename, fun)
  local src = io.input(filename)
  while true do
    local str = io.read()
    if not str then return io.close(src) else fun(str) end end end

-- Read lines from `filestr`, converting each into words, passing that to `fun`.
function csv(filename, fun)
  lines(filename, function(t) fun(words(t,",",coerce)) end) end

-- Read `filename` into a DATA object. Return that object.
function csv2data(filename,data)
  data=DATA()
  csv(filename, function(t) data:add(t) end)
  return data end

---- ---- ---- ---- Tests
-- Tests fail if they do not return `true`.
function go.the() chat(the); return true end

function go.nom(   nom)
  nom=NOM()
  for i=1,1 do 
    for _,x in pairs{"a","a","a","a","b","b","c"} do
      nom:add(x) end end
  return "a"==nom:mid() and 1.38==rnd(nom:div(),2) end

function go.ratio(    r)
  r=RATIO()
  the.ratios = 64
  for i=1,100 do r:add(i) end
  return 52==r:mid() and 32.56==rnd(r:div(),2)  end

function go.about()
  map(  ABOUT{"Clndrs","Volume","Hp:","Lbs-",
          "Acc+","Model","origin","Mpg+"}.y , chat)
  return true end

function go.one(     data1,data2)
  data1=csv2data("../../data/auto93.csv")
  print("mid1", cat(data1:mid(2)))
  print("div1", cat(data1:div(2)))
  print(1)
  data2=            data1:clone(data1.rows)
  print(2)
  print("mid2", cat(data2:mid(2)))
  print("div2", cat(data2:div(2)))
  return true
  end

function go.dist(    data,row1,row2)
  data= csv2data("../../data/auto93.csv")
  print(#data.rows)
  for i = 1,20 do
    row1=any(data.rows)
    row2=any(data.rows)
    print(dist(row1,row2)) end
  return true end

function go.betters(   data,data1,data2)
  data= csv2data("../../data/auto93.csv")
  data.rows = sort(data.rows, better) 
  data1=clone(data, slice(data.rows,1,50))
  data2=clone(data, slice(data.rows,(#data.rows)-50))
  map({stats(data1),stats(data2)},chat)
  return true end

function go.half(   data)
  data= csv2data("../../data/auto93.csv")
  local As,Bs,_= half.splits(data.rows)
  print(#As,#Bs)
  return true end

function go.bestOrRest(   data,data1,data2,best,rest0,rest)
  data= csv2data("../../data/auto93.csv")
  best,rest0 = bestOrRest(data.rows)
  rest = many(rest0, #best*4)
  data1=clone(data, best)
  data2=clone(data, rest)
  map({stats(data1),stats(data2)},chat) 
  return true end

function go.bins()
  local data= csv2data("../../data/auto93.csv")
  local best,rest0 = half.splits(data.rows)
  local rest = many(rest0, #best*2*the.Balance)
  local rows ={}
  for _,row in pairs(rest) do push(rows,row).label="rest" end
  for _,row in pairs(best) do push(rows,row).label="best" end
  for _,col in pairs(data.about.x) do
    print("")
    map(bins.find(rows,col),
        function(xy) print(xy.txt,xy.xlo,xy.xhi, cat(xy.y.has)) end) end
  return true end

local _ranked=function(data)
   for n,row in pairs(sort(data.rows,better)) do row.rank= rnd(100*n/#data.rows,0); end
   for _,row in pairs(data.rows) do row.evaled=false end
   shuffle(data.rows)
   return data  end

function go.rules(      data)
  local data= _ranked(csv2data("../../data/auto93.csv"))
  how.rules(data)
  chat(sort(map(data.rows,get"rank")))
  return true end
    
---- ---- ---- ---- Start-up
-- Counter for test failures
local fails=0

-- Run one test. Beforehand, reset random number seed. Afterwards,
-- reset the settings to whatever they were before the test.
local function run(str)
  if type(go[str])=="function" then
    local saved={};for k,v in pairs(the) do saved[k]=v end
    math.randomseed(the.seed)
    if true ~= go[str]() then fails=fails+1; print("FAIL",str) end
    for k,v in pairs(saved) do the[k]=v end  end end

if pcall(debug.getlocal,4,1) then -- If code loaded via a `require` statement,
  return XAI                      -- Then just return the names.
else                              -- Else...
   the = cli(the)                                           -- update settings
   local todo ={}; for k,_ in pairs(go) do push(todo,k) end -- Run tests.
   for _,k in pairs(the.go=="all" and sort(todo) or {the.go}) do run(k) end
   rogues()       -- Check for rogue local.
   os.exit(fails) -- Report failures were seen.
end
