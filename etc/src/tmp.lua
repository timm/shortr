-- SHORTr : less (but better) XAI
--    
--    
-- AI and XAI (explainable artificial intelligence) need not be
-- hard.  E.g. here's a few hundred lines of LUA
-- to search N items to  find and explain the best ones, using just
-- log(N) evals.  
--    
-- <a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a><br>
-- <a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a><br>
-- <a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a><br>
-- <a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a><br>
-- <a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
-- <br clear=all>
--    
--    
local help=[[

TINYXAI.lua: dont fuse on non-numerics

USAGE:
  lua tinyxai.lua [OPTIONS]

OPTIONS:
 -b bins     max number of bins    = 16
 -c cohen    difference in nums    = .35
 -f file     source                = ../../data/auto93.csv
 -F far      how far is far        = .95
 -g go       action                = help
 -h help     show help             = false
 -m min      size of small         = .5
 -p p        distance coefficient  = 2
 -S samples  samples               = 64
 -s seed     random number seed    = 10019]]
-- ## Names


-- #### Locals


-- Store old names (so, on last line, we can check for rogue locals)
local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end
-- Define new names from library code (so we can mention them before defining them).
local any,big,cat,chat,coerce,csv,data,fmt,lt
local map,max,min,per,push,rand,shuffle,sort
-- Place to store test suites (to disable a test, move it `go` to `no`.
local go,no = {},{}
-- Place to store the settings (and this variable is parsed from help text; e.g. `the.bins=16`).
local the = {}

-- ####  Objects


--**obj(str,fun): class**<br>`Fun` is a constructor for instances of class `str`.
-- BTW, polymorphism, encapsulation, classes, instance, constructors, all  in 3 lines. :-)
local function obj(txt,fun,  t,i) 
  local function new(k,...) i=setmetatable({},k); fun(i,...); return i end
  t={__tostring = cat}; t.__index = t;return setmetatable(t,{__call=new}) end

--**ROW(tab)**<br>Stores one record. ROWs are stored in a contained called ROWS.
-- Implementation note: ROWs are created when data is read from CSV files.
-- After that, if a ROW is added to more than one ROWS object then the same
-- ROW will be held in different ROWSs. This makes certain labelling and
-- record keep tasks easier (e.g. tracking how many rows we have evaluated).
local ROW=obj("ROW", function(self,cells) 
  self.cells = {}           -- place to hold one record
  self.label  = false       -- true if we have decided  this ROW is "best"?
  self.evaled = false end)  -- have we accessed this row's y-values?

--**SYM(?num=0, ?str="")**<br>Summarizes streams of symbols in ROWs
local SYM=obj("SYM", function(self,at,txt) 
  self.n   = 0          -- number of items seen
  self.at  = at or 0    -- column number
  self.txt = txt or ""  -- column name
  self.kept= {}   end)  -- counters for symbols

--**NUM(?num=0, ?str="")**<br>Summarize streams of numbers in ROWs
local NUM=obj("NUM", function(self,at,txt) 
  self.n   = 0                        -- number of items seen
  self.at  = at   or 0                -- column number
  txt=txt or ""
  self.txt = txt                      -- column name
  self.w   = txt:find"-$" and -1 or 1 -- If minimizing, then -1. Else 1
  self.kept= {}                       -- some sample of the seen items
  self.ok  = false end)               -- true if sorted, set to false by each add

--**COLS([str]+)**<br>Factory for making NUMs or SYMs from list of col names.
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
  for k,v in pairs(names) do
    col= push(self.all, (v:find"^[A-Z]" and NUM or SYM)(at,txt))
    if not v:find":$" then
      if v:find"!$" then self.klass = col end
      push(v:find"[!+-]$" and self.y or self.x, col) end end end)

--**ROWS()**<br>Stores `rows` and their summaries in `cols`.
local ROWS=obj("ROWS", function(self) 
  self.rows = {}        -- [ROW] records, stored as ROW
  self.cols = nil end)  -- a COLS instance (if nil, no data read yet)

--**BIN( NUM|SYM, num, ?num=lo, ?SYM)**<br>Values from same rows in 2 columns
local BIN=obj("BIN",function(self,col, lo, hi, has) 
  self.col = col               -- What column does this bin handle?
  self.lo  = lo                -- Lowest value of column1.
  self.hi  = hi or lo          -- Highest value of column1
  self.has = has or SYM() end) -- Symbol counts of column2 values.

-- ##  Columns


-- Columns summarize sets of rows.
-- ### Sym


-- #### Create


--**`SYM`:  merge(SYM): SYM**<br>Create a new SYM by merging two others.
function SYM:merge(other,    k)
  k= SYM(self.at, self.txt)
  for x,n in pairs(self.kept)  do k:add(x,n) end
  for x,n in pairs(other.kept) do k:add(x,n) end
  return k end

-- #### Update


--**`SYM`:  add(any,?num=1)**<br>Add a symbols `x`. Do it `n` times.
function SYM:add(x,n) 
  n = n or 1
  if x~="?" then self.n=self.n+n; self.kept[x]=n + (self.kept[x]+0) end end

-- #### Query


--**`SYM`:  div():num**<br>Diversity. Return entropy.
function SYM:div() 
  return sum(self.kept, function(n) return -n/i.n*math.log(n/i.n,2) end) end

-- ####  Distance


--**`SYM`:  dist(atom,atom):num**<br>Identical symbols have distance 0. Otherwise, 1.
-- If any unknowns, assume max distance.
function SYM:dist(x,y) return (x=="?" or  y=="?") and 1 or x==y and 0 or 1 end

-- #### Discretization


--**`SYM`:  bin(any):any -0 Discretize a symbol (do nothing)**<br>
function SYM:bin(x) return x end
--**`SYM`:  merges(t,...):SYM**<br>Merge adjacent bins (do nothing: SYM ranges dont't merge)
function SYM:merges(t,...) return t end

-- ### Num


-- #### Update


--**`NUM`:  add(num)**<br>Add `x`. If no space, at prob `some/n`, replace any old number.
function NUM:add(x)
  if x~="?" then 
    self.n = self.n + 1
    local pos
    if #self.kept < the.some        then pos= #i.kept+1 
    elseif rand() < the.some/self.n then pos= rand(#i.kept) end
    if pos then 
      self.ok=false  -- the `kept` list is no longer in sorted order
      self.kept[pos]=x end end end

-- #### Query


--**`NUM`:  has()**<br>Return `kept`, ensuring it is sorted.
function NUM:has()
  self.kept = self.ok and self.kept or sort(self.kept)
  self.ok = true
  return self.kept end

--**`NUM`:  norm(x):num**<br>Normalize x,y to 0..1.
function NUM:norm(x)
  local a =  self:has()
  local lo,hi = a[1], a[#a]
  return x=="?" and x or math.abs(hi-lo)<1E-9 and 0 or (x-lo)/(hi-lo+1/big) end

-- #### Distance


--**`NUM`:  dist(x,y):num**<br>Normalize x,y to 0..1, report their difference.
-- If any unknowns, assume max distance.
function NUM:dist(x,y)
  if x=="?" and y=="?" then return 1
  elseif x=="?" then y=self:norm(y); x=y<.5 and 1 or 0 
  elseif y=="?" then x=self:norm(x); y=x<.5 and 1 or 0
  else   x,y = self:norm(x), self:norm(y) end
  return math.abs(x-y) end

-- #### Discretization


--**`NUM`:  bin(any):any**<br>Discretize a num to one of `the.bins`.
function NUM:bin(x)
  local a = self:has()
  local lo,hi = a[1], a[#a]
  local b = (hi - lo)/the.bins
  return hi==lo and 1 or math.floor(x/b+.5)*b end

--**`NUM`:  merges([BIN],...) :[BIN]**<br>Prune superflous bins.
function NUM:merges(b4, min) 
  local n,now = 1,{}
  while n <= #b4 do
    local merged = n<#b4 and b4[n]:merged(b4[n+1],min) -- defined in BIN
    now[#now+1]  = merged or b4[n]
    n            = n + (merged and 2 or 1)  -- if merged, skip over merged bin
  end -- end while
  if #now < #b4 then return self:merges(now,min) end     -- seek others to merge
  bins[1].lo,bins[#bins].hi = -big,big            -- grow to plus/minus infinity
  return bins end 

-- ## Data


-- ### Meta


-- ####  Update


--**`COLS`:  add(ROW)**<br>update the non-skipped columns with values from ROW
function COLS:add(row)
  for _,cols in pairs{self.x, self.y} do 
    for _,col in pairs(cols) do col:add(row.cells[col.at]) end end end

-- #### Distance


--**`COLS`:  dist(ROW,ROW) :num**<br>Using `x` columns, compute distance.
function COLS:dist(r1,r2)
  local d,x1,x2 = 0
  for _,col in pairs(self.x) do 
    x1 = r1.cells[col.at]
    x2 = r2.cells[col.at]
    d  = d+(col:dist(x1,x2))^the.p end
  return (d/#self.x)^(1/the.p) end

-- function COLS:half(rows,b4)
  local tmp, abc, i, xCs = {}
  abc = function(A,B) return {A=A, B=B, As={}, Bs={}, c=self:dist(A,B)} end
  for n=1,the.samples do push(tmp, abc(b4 or any(rows), any(rows))) end
  i   = per(sort(tmp,lt"c"), the.far)
  xCs = function(C)     
          return {x = (self:dist(C,i.A)^2+i.c^2-self:dist(C,i.B)^2)/(2*i.c),
                  C = C} end
  for j,xC in pairs(sort(map(rows,xCs),lt"x")) do
    push(j<#rows/2 and i.As or i.Bs, xC.C) end 
  return i end 
-- #### Optimization

-- -> COLS:best(ROW,ROW) :bool -> Multi-objective comparisons. True if moving to self losses least than moving to other.
function COLS:best(r1,r2)
  r1.evaled,r2.evaled = true,true
  local s1, s2, ys, e = 0, 0, self.y, math.exp(1)
  for _,col in pairs(ys) do
    local x = col:norm(r1.cells[col.at])
    local y = col:norm(r2.cells[col.at])
    s1      = s1 - e^(col.w * (x-y)/#ys)
    s2      = s2 - e^(col.w * (y-x)/#ys) end
  return s1/#ys < s2/#ys  end

-- -> COLS:bests([ROW]):bests=[ROW],rests=[ROW] -> Recursively apply `best`. Return most preferred rows, and the rest.
function COLS:bests(rows,    b4,stop,rests)
  rests = rests or {}
  stop  = stop or (#rows)^the.min
  if #rows < stop then return rows,rests end -- return best=[ROW],rests=[ROW] 
  local two = self:half(rows,b4) -- if b4 supplied, then half will use it as one pole.
  local best, bests, rests1
  if   self:best(two.A, two.B) 
  then best, bests, rests1 = two.A, two.As, two.Bs
       for i=#rests1,1,-1 do push(rests, rests1[i]) end -- rests sorted, L to R, worst to better
  else best, bests, rests1 = two.B, two.Bs, two.As
       for i=1,#rests1, 1 do push(rests, rests1[i]) end -- rests sorted, L to R, worst to better
  end
  return self:best(bests, best, stop,rests) end

function BIN:add(x,y)
  self.lo = min(x,self.lo)
  self.hi = max(x,self.hi)
  self:has(y) end

function BIN:merged(j, min)
  local a, b, c = self.has, j.has, self.has:merge(j.has)
  local should = a.n < min or b.n < min  
  local can    = c:div() <= (a.n*a:div() + b.n*b:div())/c.n 
  if should or can then return BIN(a.col,self.lo, j.hi, c) end end

function BIN:selects(rows,    select,tmp)
  function select(row,  v)
    v= row.cells[self.col.at]
    if v=="?" or self.lo==self.hi or self.lo<v and v <=self.hu then return row end end
  tmp= map(rows,select) 
  if #tmp < #rows then return rows end end

-- XXX start here
function ROWS:tree(gaurd,stop)
  self.gaurd = gaurd.
  stop = stop or (#self.rows)^the.min
  bests,rests= cols:bests(rows)
  for i=3*#bests+1,#rests do rests[i]= nil end
  local binsDiv = function(col) return constrast(col,bests,rests) end
  cols.kids= map(sort(map(cols.x,binsDiv),lt"div")[1].bins,
                 function(bin) 
                   local tmp= bin:selects(row),bin)
                   if tmp then return self:clone(tmp):tree(bin,stop) end end ) end
      
function constrast(col,rows)
  for _,row in pairs(rows) do
    local v = row[col.at]
    if v ~= "?" then
      n=n+1
      local pos = col:bin(v)
      dict[pos] = dict[pos] or push(list, BIN(col,v))
      dict[pos]:add(v,row.label) end end 
  list = col:merges(sort(list,lt"lo"), n^the.min)
  return { bins=list,
           div=sum(list,function(z) return z.has:div()*z.ys.n/n end)} end 
-------------------------------------------------------------------------------
function ROWS:file(x) for t in csv(x) do self:add(t) end; return self end

function ROWS:adds(t) for _,t1 in pairs(t) do self:add(t1) end; return self end

function ROWS:add(t) 
  if   self.cols 
  then self.cols:add(push(i.rows, t.cells and t or ROW(t))) 
  else self.cols=COLS(t) end end

------------------------------------------------------------------------------
-- ## Lib
big=math.huge 
min=math.min
max=math.max
fmt=string.format
rand=math.random

function any(a)  return a[rand(#a)] end
function per(t,p) p=p*#t//1; return t[math.max(1,math.min(#t,p))] end

function push(t,x) t[1+#t]=x; return x end
function map(t,f,     u) u={};for _,x in pairs(t) do u[1+#u]=f(x)end;return u end
function sum(t,f,     u) u=0; for _,x in pairs(t) do u=u+f(x)    end;return u end

function sort(t,f) table.sort(t,f); return t end
function lt(x)     return function(a,b) return a[x] < b[x] end end

function shuffle(t,   j)
  for i=#t,2,-1 do j=rand(i); t[i],t[j]=t[j],t[i] end; return t end

function coerce(x)
  x = x:match"^%s*(.-)%s*$" 
  if x=="true" then return true elseif x=="false" then return false 
  else return math.tointeger(x) or tonumber(x) or x end  end

function chat(t) print(cat(t)) return t end 
function cat(t,   show,u)  
  function show(k,v) return #t==0 and (":%s %s"):format(k,v) or tostring(v) end
  u={}; for k,v in pairs(t) do u[1+#u]=show(k,v) end
  return (t._is or "").."{"..table.concat(#t==0 and sort(u) or u," ").."}" end

function csv(file,fun)
  function lines(file, fun)
    local file = io.input(file)
    while true do
      local line = io.read()
      if not line then return io.close(file) else fun(line) end end 
  end -----------------------------
  function words(s,sep,fun,      t)
     fun = fun or same
     t={};for x in s:gmatch(fmt("([^%s]+)",sep)) do t[1+#t]=fun(x) end; return t 
  end -------------------------------------------------------------
  lines(file, function(line) fun(words(line, ",", coerce)) end) end 
-- -----------------------------------------------------------------------------
-- ## Start-up
function go.the() chat(the) end

help:gsub("\n ([-]%S)[%s]+([%S]+)[^\n]+= ([%S]+)",function(f,key,x) 
  for n,y in ipairs(arg) do if y==f then 
    x = x=="false" and "true" or x=="true" and "false" or arg[n+1] end end
  the[key] = coerce(x) end) 

if the.help then os.exit(print(help:gsub("[%u][%u%d]+","\27[1;32m%1\27[0m"),"")) end 
math.randomseed(the.seed)
if go[the.go] then go[the.go]() end
for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end
