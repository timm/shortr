-- <span id="forkongithub"><a href="https://github.com/timm/shortr">Fork me on GitHub</a></span>
--
-- <em>"If you cannot - in the long run - (explain to)
--     everyone what you have been doing,
--     your doing  has been worthless."</em><br>- Erwin Schrodinger
--
-- <em>And I say, hey-ey-ey-a-ey, hey-ey-ey-ey
--     I said "Hey, a-what's going on?"</em> <br>- 4 Non Blondes
--
-- AI and XAI (explainable artificial intelligence) need not be
-- hard.  E.g. here's a few hundred lines of LUA
-- to search N items to  find and explain the best ones, using just
-- log(N) evals.  
--
-- This code makes extensive use of a ROWS object.  Data from disk
-- becomes a ROWS. ROWS are recursive bi-clustered by partitioning on
-- the distance to two distant points (found after a few dozen random
-- projections).  Each cluster is new ROWS object, containing a subset
-- of the data. A decision tree is built that reports the difference
-- between the "best" and "worst" clusters (defined using a multi-objective
-- domination predicate) and that tree is just a  ser
-- of ROWS with `kids` pointer to sub-RWS).  This process
-- only needs log2(N) queries to y-values (while clustering,
-- just on the pairs of
-- distance objects).
--
-- This code starts with a help string (from which we extract our global settings)
-- and ends with a library of demos (see the `go` functions at end of file).  
-- Each setting can be (optionally) updated by a command-line flag.
-- Demos can be run separately or  all at once (using `-g all`).
--   For regression tests, we report the failures seen when the demos run.
--
-- <a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
-- <[a](a) href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
-- <a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a><br>
-- <a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
-- <br clear=all>
--    
local help=[[

XPLAN.lua: semi-supervised multi-objective explanation 
(c)2022, Tim Menzies <timm@ieee.org>, BSD-2 license

SYNOPSIS:
  Finds sqrt(N) best things using log2(N) evaluations.

DESCRIPTION:
  Data, with multiple dependent goals, is recursive
  bi-clustered on independent variables by partitioning
  on the distance to two distant points (found via
  random projections).  The delta between best and worst
  clustered (defined via a multi-objective comparison)
  is then reported as a decision tree after discretizing
  numerics to best distinguish best and worst.
  This process only makes log2(N) queries to y-values
  (while clustering, just on the pairs of distance objects).

USAGE:
  lua xplaning.lua -bcfFghmprsS arg

OPTIONS:
 -b bins         max number of bins    = 16
 -c cohen        difference in nums    = .35
 -f file         source                = ../../data/auto93.csv
 -F Far          how far is far        = .95
 -g go           action                = nothing
 -h help         show help             = false
 -m min          size of small         = .5
 -p p            distance coefficient  = 2
 -r rests        number of rests to use= 4
 -s seed         random number seed    = 10019
 -S Some         how many nums to keep = 512

FILES:
  xplain.lua    : main code
  xplaining.lua : examples of how to use code.
  lib.lua       : misc LUA support utilities
  data/*.csv    : misc example data files
  Makefile      : not needed for execution (useful for doco)

EXIT STATUS:
  Non-zero only if any start-up actions fail.]]

---- ---- ---- ---- Names
---- ---- Locals
-- Place for names from library
local l=require"lib"
local any,big,cat,chat,coerce,csv   = l.any,l.big,l.cat,l.chat,l.coerce,l.csv
local fmt,kap,lt,many,map,max,min   = l.fmt,l.kap,l.lt, l.many,l.map,l.max,l.min
local obj,per,push,rand,rev,rnd,rnds= l.obj,l.per,l.push,l.rand,l.rev,l.rnd,l.rnds
local shuffle,sort,sum              = l.shuffle,l.sort,l.sum 
-- Place for settings (parsed from help text; e.g. `the.bins=16`).
local the = {}

---- ----  Objects
---- ROW(tab) -- Stores one record. ROWs are stored in a contained called ROWS.
-- Implementation note: ROWs are created when data is read from CSV files.
-- After that, if a ROW is added to more than one ROWS object then the same
-- ROW will be held in different ROWSs. This makes certain labelling and
-- record keep tasks easier (e.g. tracking how many rows we have evaluated).
local ROW=obj("ROW", function(self,cells) 
  self.cells = cells        -- place to hold one record
  self.label  = false       -- true if we have decided  this ROW is "best"?
  self.evaled = false end)  -- have we accessed this row's y-values?

---- SYM(?num=0, ?str="") -- Summarizes streams of symbols in ROWs 
local SYM=obj("SYM", function(self,at,txt) 
  self.n   = 0          -- number of items seen
  self.at  = at or 0    -- column number
  self.txt = txt or ""  -- column name
  self.kept= {}   end)  -- counters for symbols

---- NUM(?num=0, ?str="") -- Summarize streams of numbers in ROWs
local NUM=obj("NUM", function(self,at,txt) 
  self.n   = 0                        -- number of items seen
  self.at  = at   or 0                -- column number
  txt=txt or ""
  self.txt = txt                      -- column name
  self.w   = txt:find"-$" and -1 or 1 -- If minimizing, then -1. Else 1
  self.kept= {}                       -- some sample of the seen items
  self.ok  = false end)               -- true if sorted, set to false by each add

----  COLS([str]+) -- Factory for making NUMs or SYMs from list of col names.
-- Column names starting with upper case are NUMs (others are SYMs).
-- Anything adding with "!+-" is a dependent goal column.
-- Column names ending with "`:`" are "skipped"; i.e. 
-- not added to the list of independent or dependent columns.
local COLS=obj("COLS", function(self,  names)
  self.names= names -- list of column names
  self.all  = {}    -- [NUM|SYM] all names, converted to NUMs or SYMs
  self.x    = {}    -- [NUM|SYM] just the independent columns
  self.y    = {}    -- [NUM|SYM] just the dependent columns
  self.klass= nil   -- SYM       the klass column (if it exists)
  for at,txt in pairs(names) do
    local col= push(self.all, (txt:find"^[A-Z]" and NUM or SYM)(at,txt))
    if not txt:find":$" then
      if txt:find"!$" then self.klass = col end
      push(txt:find"[!+-]$" and self.y or self.x, col) end end end)

---- ROWS() -- Stores `rows` and their summaries in `cols`.
local ROWS=obj("ROWS", function(self) 
  self.rows = {}        -- [ROW] records, stored as ROW
  self.cols = nil end)  -- a COLS instance (if nil, no data read yet)

---- BIN( NUM|SYM, num, ?num=lo, ?SYM) -- Values from same rows in 2 columns
local BIN=obj("BIN",function(self,col, lo, hi, has) 
  self.col = col               -- What column does this bin handle?
  self.lo  = lo                -- Lowest value of column1.
  self.hi  = hi or lo          -- Highest value of column1
  self.has = has or SYM(col.at,col.txt) end) -- Symbol counts of column2 values.


---- ---- ---- ---- Columns
---- ---- ---- Sym
---- ---- Create
---- SYM:merge(SYM): SYM --  Create a new SYM by merging two others.
function SYM:merge(other,    k)
  k= SYM(self.at, self.txt)
  for x,n in pairs(self.kept)  do k:add(x,n) end
  for x,n in pairs(other.kept) do k:add(x,n) end
  return k end

---- ---- Update
---- SYM:add(any,?num=1) -- Add a symbols `x`. Do it `n` times.
function SYM:add(x,n) 
  n = n or 1
  if x~="?" then self.n=self.n+n; self.kept[x]=n + (self.kept[x] or 0) end end

---- ---- Query
----  SYM:div():num -- Diversity. Return entropy.
function SYM:div() 
  return sum(self.kept, function(n) return -n/self.n*math.log(n/self.n,2) end) end

---- SYM:mid():num -- Return `mid`dle (mode) symbol.
function SYM:mid() 
  local most,mode = -1,nil
  for x,n in pairs(self.kept) do if n>most then most,mode=n,x end end
  return mode end

---- ----  Distance
---- SYM:dist(atom,atom):num -- Identical symbols have distance 0. Otherwise, 1.
-- If any unknowns, assume max distance.
function SYM:dist(x,y) return (x=="?" or  y=="?") and 1 or x==y and 0 or 1 end

---- ---- Discretization
---- SYM:bin(any):any -- Discretize a symbol (do nothing)
function SYM:bin(x) return x end
---- SYM:merges(t,...):SYM --  Merge adjacent bins (do nothing: SYMs don't merge)
function SYM:merges(t,...) return t end

---- ---- ---- Num
---- ---- Update
---- NUM:add(num) -- Add `x`. If no space, at prob `some/n`, replace any old number.
function NUM:add(x)
  if x~="?" then 
    self.n = self.n + 1
    local pos
    if #self.kept < the.Some        then pos= (#self.kept)+1 
    elseif rand() < the.Some/self.n then pos= rand(#self.kept) end
    if pos then 
      self.ok=false  -- the `kept` list is no longer in sorted order
      self.kept[pos]=x end end end

---- ---- Query
----  NUM:has() -- Return `kept`, ensuring it is sorted.
function NUM:has()
  self.kept = self.ok and self.kept or sort(self.kept)
  self.ok = true
  return self.kept end

---- NUM:mid():num -- Return `mid`dle (median) number.
function NUM:mid() return per(self:has(),.5) end

---- NUM:div():num -- Return `div`ersity of numbers.
function NUM:div(  a) a=self:has(); return (per(a,.9)-per(a,.1))/2.56 end

---- NUM:norm(x):num -- Normalize x,y to 0..1.
function NUM:norm(x)
  local a =  self:has()
  local lo,hi = a[1], a[#a]
  return x=="?" and x or math.abs(hi-lo)<1E-9 and 0 or (x-lo)/(hi-lo+1/big) end

---- ---- Distance
---- NUM:dist(x,y):num -- Normalize x,y to 0..1, report their difference.
-- If any unknowns, assume max distance.
function NUM:dist(x,y)
  if x=="?" and y=="?" then return 1
  elseif x=="?" then y=self:norm(y); x=y<.5 and 1 or 0 
  elseif y=="?" then x=self:norm(x); y=x<.5 and 1 or 0
  else   x,y = self:norm(x), self:norm(y) end
  return math.abs(x-y) end

---- ---- Discretization
---- NUM:bin(any):any -- Discretize a num to one of `the.bins`.
function NUM:bin(x)
  local a = self:has()
  local lo,hi = a[1], a[#a]
  local b = (hi - lo)/the.bins
  return hi==lo and 1 or math.floor(x/b+.5)*b end

---- NUM:merges([BIN],...) :[BIN] -- Prune superflous bins. 
function NUM:merges(b4, min) 
  local n,now = 1,{}
  while n <= #b4 do
    local merged = n<#b4 and b4[n]:merged(b4[n+1],min) -- defined in BIN
    now[#now+1]  = merged or b4[n]
    n            = n + (merged and 2 or 1)  -- if merged, skip over merged bin
  end -- end while
  if #now < #b4 then return self:merges(now,min) end     -- seek others to merge
  now[1].lo, now[#now].hi = -big,big            -- grow to plus/minus infinity
  return now end 

---- ---- ---- ---- Data
---- ---- ---- COLS
---- ---- Update
---- COLS:add(ROW) --  update the non-skipped columns with values from ROW
function COLS:add(row)
  for _,cols in pairs{self.x, self.y} do 
    for _,col in pairs(cols) do col:add(row.cells[col.at]) end end end

---- ---- Distance
---- COLS:dist(ROW,ROW) :num --  Using `x` columns, compute distance.
function COLS:dist(r1,r2)
  local d,x1,x2 = 0
  for _,col in pairs(self.x) do 
    x1 = r1.cells[col.at]
    x2 = r2.cells[col.at]
    d  = d+(col:dist(x1,x2))^the.p end
  return (d/#self.x)^(1/the.p) end

-- To avoid outliers only look so `Far` down the list of neighbors
-- (sorted by distance to `r1`).
function COLS:far(r1, rows)
  local tmp = map(rows, function(r2) return {r=r2, d=self:dist(r1,r2)} end)
  return per(sort(tmp, lt"d"), the.Far).r end

---- COLS:half([ROW]) -- Divide `rows` by their distance to two distant points.
-- Find two distant points A,B using a random projections. The half of the
-- the rows nearest `A` are returned as `As` (ditto with `B` and `Bs`).
function COLS:half(rows,   b4)
  local As,Bs= {},{}
  local some = many(rows, the.Some)
  local A    = b4 or self:far(any(some), some)
  local B    = self:far(A, some)
  local c    = self:dist(A,B) 
  rows       = map(rows, function(row,     a,b) 
                           a,b = self:dist(row,A), self:dist(row,B) 
                           return {r= row, x= (a^2 + c^2 - b^2)/(2*c)} end)
  for j,rx in pairs(sort(rows,lt"x")) do push(j<#rows/2 and As or Bs, rx.r) end  
  return {A=A, B=B, As=As, Bs=Bs, c=c}  end

---- ---- Optimize
----  COLS:best(ROW,ROW) :bool -- True if better on multi-objectives
function COLS:best(r1,r2)
  r1.evaled, r2.evaled = true, true
  local s1, s2, ys, e = 0, 0, self.y, math.exp(1)
  for _,col in pairs(ys) do
    local x = col:norm(r1.cells[col.at])
    local y = col:norm(r2.cells[col.at])
    s1      = s1 - e^(col.w * (x-y)/#ys)
    s2      = s2 - e^(col.w * (y-x)/#ys) end
  return s1/#ys < s2/#ys end -- i.e. we lose less going to r2->r1 than r2->r1

---- COLS:bests([ROW]):bests=[ROW],rests=[ROW] -- Recursively apply `best`.
-- Returns `bests` and everything else as `rest`.
function COLS:bests(rows,    stop,b4,rests,evals)
  rests = rests or {}
  evals = evals or 0
  stop  = stop or (#rows)^the.min
  if #rows < stop then return rows,rests,evals end -- return best=[ROW],rests=[ROW] 
  local two = self:half(rows,b4) -- if b4 supplied, then half will use it as one pole.
  local best, bests, rests1
  if   self:best(two.A, two.B) 
  then best, bests, rests1 = two.A, two.As, two.Bs
       for _,rest1 in pairs(rev(rests1)) do push(rests,rest1) end --sort L to R, worst to better
  else best, bests, rests1 = two.B, two.Bs, two.As
       for _,rest1 in pairs(rests1) do push(rests,rest1) end --sort L to R, worst to better
  end
  return self:bests(bests, stop, best,rests, evals+1) end

function COLS:ranks(rows, n)
  n=NUM(); for _,row in pairs(rows) do n:add(row.rank) end; return n end

function COLS:rank(rows)
  for j,row in pairs(sort(rows, function(r1,r2) return self:best(r1,r2) end)) do
    row.evaled = false
    row.rank = (100 * j /#rows) // 1  end
  return shuffle(rows) end 

---- ---- ---- ---- COLS
---- ---- ---- BINS
---- ---- Create
---- BIN:merged(BIN, num) -- Combine two bins if we should or can do so.
-- "Should" is true if either is too small.   
-- "Can" is true of the whole is simpler than the parts.
function BIN:merged(j, min)
  local a, b, c = self.has, j.has, self.has:merge(j.has)
  local should = a.n < min or b.n < min                 
  local can    = c:div() <= (a.n*a:div() + b.n*b:div())/c.n 
  if should or can then return BIN(a.col,self.lo, j.hi, c) end end

---- ---- Update
---- BIN:add(num,sym) -- extend `lo,hi` to cover `x`; remember we saw `y.
function BIN:add(x,y)
  self.lo = min(x,self.lo)
  self.hi = max(x,self.hi)
  self.has:add(y) end

---- ---- Query
---- BIN:__tostring() -- pretty print the range
function BIN:__tostring()
  local x,lo,hi = self.has.txt, self.lo, self.hi
  if     lo ==  hi  then return fmt("%s == %s", x, lo)
  elseif hi ==  big then return fmt("%s >  %s", x, lo)
  elseif lo == -big then return fmt("%s <= %s", x, hi)
  else                   return fmt("%s <  %s <= %s", lo,x,hi) end end

---- BIN:selects([ROW]):[ROW] -- Returns rows that fall within this BIN. 
-- Returns nil if the subset is same size as original sets.
function BIN:selects(rows,    select,tmp)
  function select(row,  v)
    v= row.cells[self.col.at]
    if v=="?" or self.lo==self.hi or self.lo<v and v <=self.hu then return row end end
  tmp= map(rows,select) 
  if #tmp < #rows then return rows end end

---- ---- ROWS
function ROWS:clone(t) return ROWS():add(self.cols.names):adds(t or {}) end

function ROWS:file(x)  csv(x,function(t) self:add(t) end); return self end

function ROWS:adds(t) for _,t1 in pairs(t) do self:add(t1) end; return self end

function ROWS:add(t) 
  if   self.cols 
  then self.cols:add(push(self.rows, t.cells and t or ROW(t))) 
  else self.cols=COLS(t) end 
  return self end

-- ROWS:mids(?int=2,?[COL]=self.cols.y):[key=num] -- Return `mid` of columns
-- rounded to `p` places.
function ROWS:mid(p,cols) 
  local t={n=#self.rows}
  for _,col in pairs(cols or self.cols.y) do t[col.txt]=col:mid(p) end
  return rnds(t,p or 2) end

function ROWS:bins(rows,col)
  local n,dict,list = 0,{},{}
  for _,row in pairs(rows) do
    local v = row.cells[col.at]
    if v ~= "?" then
      n=n+1
      local pos = col:bin(v)
      dict[pos] = dict[pos] or push(list, BIN(col,v))
      dict[pos]:add(v,row.label) end end 
  list = col:merges(sort(list,lt"lo"), n^the.min)
  print("dict",cat(dict),"sum",sum(list,function(z) return z.has:div()*z.has.n/n end))  
  return {bins = list,
          div  = sum(list,function(z) return z.has:div()*z.has.n/n end)} end

function ROWS:splitter(rows)
  local binsDiv = map(self.cols.x, function(col) return self:bins(rows,col) end)
  return sort(binsDiv, lt"div")[1].bins end

function ROWS:tree()
  local bests,rests,_= self.cols:bests(self.rows)
  local rows={}
  for _,best in pairs(bests) do push(rows,    best).label=1 end
  for i = 1,the.rests*#bests do push(rows,rests[i]).label=0 end
  self:grow(rows, (#rows)^the.min) 
  return self end

function ROWS:grow(rows,stop,when)
  print("rows",#rows)
  local function kid(bin)
    local t = bin:selects(rows)
    if t then return self:clone(t):grow(t,stop,bin) end end 
  self.when = when
  if #rows < stop then return self end
  self.kids = map(self:splitter(rows), kid) end
---- ---- ---- ---- Start-up

help:gsub("\n [-]%S[%s]+([%S]+)[^\n]+= ([%S]+)",function(k,x) the[k]=coerce(x)end) 

return {the=the, help=help, 
        ROWS=ROWS, ROW=ROW, COLS=COLS, NUM=NUM, SYM=SYM, BIN=BIN}
