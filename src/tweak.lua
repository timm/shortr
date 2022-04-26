-- vim: filetype=lua ts=2 sw=2 et:
-- (c) 2022, Tim Menzies,  timm@ieee.org, opensource.org/licenses/Fair
-- Usage of the works is permitted provided that this instrument is retained
-- with the works, so that any entity that uses the works is notified of this
-- instrument.  DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
--     __                                     __         
--    /\ \__                                 /\ \        
--    \ \ ,_\   __  __  __     __      _     \ \ \/'\    
--     \ \ \/  /\ \/\ \/\ \  /'__`\  /'__`\   \ \ , <    
--      \ \ \_ \ \ \_/ \_/ \/\  __/ /\ \L\.\_  \ \ \\`\  
--       \ \__\ \ \___x___/'\ \____\\ \__/.\_\  \ \_\ \_\
--        \/__/  \/__//__/   \/____/ \/__/\/_/   \/_/\/_/

local the, help= {}, [[
tweak: try three weak leaners for multi-objective optimization
(c) 2022, Tim Menzies,  timm@ieee.org, opensource.org/licenses/Fair

learner1: 5 times, discard half the data firthers from best (so 5 evals)
learner2: classify data according to presence of "best" from learner1
learner3: run learner1 on best "best" found by learner2
"Best" is defined by Zitler's multi-objective domination predicate. 

USAGE:
  alias twk="lua tweak.lua "
  twk [OPTIONS]

OPTIONS:
  --cohen   -c  cohen                       = .35
  --K       -K  manage low class counts     = 1
  --M       -M  manage low evidence counts  = 2
  --far     -F  how far to go for far       = .9
  --p       -p  coefficient on distance     = 2
  --seed    -S  seed                        = 10019
  --some    -s  sample size for distances   = 512
  --stop    -T  how far to go for far       = 20
  --min     -m  size of min space           = .5
  --best    -B  best percent                = .05

OPTIONS (other):
  --dump    -d  dump stack+exit on error    = false
  --file    -f  file name                   = ../etc/data/auto93.csv
  --help    -h  show help                   = false
  --rnd     -r  rounding numbers            = %5.3f
  --go      -g  start up action             = nothing]]

local Code_Conventions =[[
- Settings generated from "help" string
  - Settings can be updated from the strings seed in flags
  - At start, turn setting strings to real things (via "string2thing")

- Layout code in chunks of size 120 lines (max), broken by line-feed
  - Chunk1=header; Chunk2=utils; Chunk3=objects; Chunk(last)=demos+start-up
- Layout lines 80 chars side (max)
- Do functions as one-liners (if possible)
- In order to define code in any order:
  - Near the top, define all function and Object names as "local"
- Otherwise, don't use the "local" keyword (too ugly)

- Minimize use of map (hard to debug)
- Object names are short and UPPER CASE
- Private object slots (that should not be printed) start with "_".
- Constructors need not return constructed instance.
- No inheritance (hard to debug)

- Tests  in the "go" table at end. Reset settings to defaults after each.
- Tests check for error conditions using "ok" not "assert".
- Command line "-d -go x" crashed if test "x" fails, shows stack dump.
- Command line "-go x" calls test "go.x()"
- Command line "-go all" calls all tests.
- Command line "-h" shows help
- Command line "-S N" sets random seed (so "-S $RANDOM" is "full" random)

- 2nd last line: look for "rogue" globals (there should be none)
- Last line: exit to operating system with number of failures seen in tests]]

help:gsub("\n  ([-][-]([^%s]+))[%s]+(-[^%s]+)[^\n]*%s([^%s]+)",function(f1,k,f2,x)
  for n,flag in ipairs(arg) do if flag==f1 or flag==f2 then
    x = x=="false" and"true" or x=="true" and"false" or arg[n+1] end end 
  the[k] = x end) -- not ready yet. full of strings that need `string2thing

local Details=[[
- "mean","mode" are generalized to "mid" (i.e. "mid-point")
- "standard deviation","entropy" are generalized  to "div" (i.e. "diversity")

- BIN holds the class labels seen between "lo" and "hi".
- EGS (examples) hold many ROWs, summarized in SYMbolic or NUMeric  columns. 
- COLS is a factory for turning column names into NUMs or SYMs.
  - Numeric names start with upper case
  - Goal names ending with "-" or ""+ get weights -1,1 for minimize,maximize
  - Non-numeric class names end with "!"
  - Columns to be skipped have a name ending with ":"
  - Non-skipped columns are divided into COLS.y and COLS.x (for goal and other)
- "mid" and "div" for EGS are computed recursively by "mid,div" in NUMs,SYMs
- distances between two rows is computed recursively via "dist" in NUMs,SYMs

- ROW1 is before than ROW2 (i.e. ROW1<ROW2) if its goals dominate (using the
  Zitzler continuous domination predicate doi.org/10.1145/1830483.1830578
- ROWs are recursively separated and clustered using random projections
  implemented via Faloutsos' FASTMAP method: doi.org/10.1145/568271.223812
- The distance between two ROWs (i.e. ROW1-ROW2) uses Aha's measure for
  heterogeneous data across the non-goal columns doi.org/10.1007/BF00153759
- ROWs are persistent. They are created once but many be used in many EGS.
- ROWs have a "_data" pointer where it gets "lo,hi" info needed for distances.
- For consistency's sake, ROW._data is fixed to the first
  EGS that holds that row.]]

---    _  _ ___ _ _    ____ 
---    |  |  |  | |    [__  
---    |__|  |  | |___ ___] 

local any,cells,coerce,csv,fmt,goalp,lessp,lt,many,map,median,mode
local nump,oo,o,obj,per,push,r,rnd,rnds,sort,slice,stats,string2thing
---     ._ _   o   _   _ 
---     | | |  |  _>  (_ 

r = math.random
function lt(x)        return function(a,b) return a[x] < b[x] end end

---                                     _                               
---      _  _|_  ._  o  ._    _    _     )    _|_  |_   o  ._    _    _ 
---     _>   |_  |   |  | |  (_|  _>    /_     |_  | |  |  | |  (_|  _> 
---                           _|                                 _|     
function string2thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x end

function csv(src)
  src = io.input(src)
  return function(line, row) 
    line=io.read()
    if not line then io.close(src) else
      row={}; for x in line:gmatch("([^,]+)") do row[1+#row]=string2thing(x) end
      return row end end end 

---     |  o   _  _|_   _ 
---     |  |  _>   |_  _> 

function any(a, i)    i=r()*#a//1; i=math.max(1,math.min(i,#a)); return a[i] end
function many(a,n, u) u={}; for j=1,n do        u[1+#u]= any(a) end;return u end
function map(t,f, u)  u={}; for _,v in pairs(t) do u[1+#u]=f(v) end;return u end
function per(t,p)     return t[ ((p or .5)*#t) // 1 ] end 
function push(t,x)    t[1+#t]=x; return x end
function sort(t,f)    table.sort(t,f) return t end
function slice(t,i,j,   u) 
  u={}; for k=(i or 1), (j or #t) do u[1+#u] = t[k] end return u end

---     ._   ._  o  ._   _|_ 
---     |_)  |   |  | |   |_ 
---     |                    
fmt = string.format
function oo(t) print(o(t)) end
function o(t,    u,one,hide,sorted)
  if type(t) ~= "table" then return tostring(t) end
  sorted = #t>0 -- true when array's indexes are 1,2...#t
  hide= function(k) return tostring(k):sub(1,1) == "_" end
  one = function(k,v) return sorted and tostring(v) or fmt(":%s %s",k,v) end
  u={}; for k,v in pairs(t) do if not hide(k) then u[1+#u] = one(k,v) end end
  return (t.is or "").."{"..table.concat(sorted and u or sort(u)," ").."}" end

function rnds(t,f) return map(t, function(x) return rnd(x,f) end) end
function rnd(x,f) 
  return fmt(type(x)=="number" and (x~=x//1 and f or the.rnd) or"%s",x) end

---     ._    _   |      ._ _    _   ._  ._   |_   o   _  ._ _  
---     |_)  (_)  |  \/  | | |  (_)  |   |_)  | |  |  _>  | | | 
---     |            /                   |                      

function obj(name,    t,new,str)
  function new(kl,...) local x=setmetatable({},kl); kl.new(x,...); return x end 
  t = {__tostring=o, is=name or ""}; t.__index=t
  return setmetatable(t, {__call=new}) end

---      _.        _   ._     
---     (_|  |_|  (/_  |   \/ 
---       |                /  

function cells(i,rows,here,there,    n,x)
  n ,here, there = 0, here or 1, there or #rows
  return function(   x) while here >= there  do
                          x = rows[here].cells[here]
                          here = here+1
                          if x~= "?" then n=n+1; return n,x end end end end
---    ____ ___   _ ____ ____ ___ ____ 
---    |  | |__]  | |___ |     |  [__  
---    |__| |__] _| |___ |___  |  ___] 

local SYM,BIN,NUM,COLS = obj"SYM",obj"BIN",obj"NUM",obj"COLS"
local ROW,EGS = obj"ROW",obj"EGS"
---      _      ._ _  
---     _>  \/  | | | 
---         /         
function SYM:new(pos,s) 
  self.pos, self.txt= pos or 0,s or "" 
  self.n, self.has, self.most, self.mode = 0,{},0,nil end

function SYM:add(x,inc)    
  if x ~= "?" then
    inc = inc or 1
    self.n = self.n + inc
    self.has[x] = (self.has[x] or 0) + inc
    if self.has[x] > self.most then self.most,self.mode = self.has[x],x end end 
  return x end

function SYM:mid() return self.mode end
function SYM:div(   e) 
  e=0; for _,m in pairs(self.has) do
         if m>0 then e = e-m/self.n * math.log(m/self.n,2) end end 
  return e end

function SYM:dist(x,y) return x=="?" and y=="?" and 1 or x==y and 0 or 1 end

function SYM:bins(rows,     x,n,out,has,tmp,inc)
  n,out,tmp = 0,{},{}
  function inc(x) n=n+1; return n end
  function has(x) tmp[x]=tmp[x] or BIN({txt=self.txt,  pos=self.pos, n=inc(x),
                                        lo=x ,hi=x, seen={}}) end
  for _,r in pairs(rows) do 
    x = r.cells[self.pos]; has(x); push(tmp[x].seen,r.klass) end
  for _,x in pairs(tmp) do push(out, x) end
  return out end

---     |_   o  ._  
---     |_)  |  | | 

function BIN:new(t) 
  self.pos, self.txt, self.n, self.has = t.pos, t.txt, t.n, {}
  self.lo, self.hi, self.y = t.lo, t.hi, t.y or SYM() end

function BIN:__tostring()
  local x,lo,hi,big = self.txt, self.lo, self.hi, math.huge
  if     lo ==  hi  then return fmt("%s == %s",x, lo)  
  elseif hi ==  big then return fmt("%s >= %s",x, lo)  
  elseif lo == -big then return fmt("%s < %s", x, hi)  
  else                   return fmt("%s <= %s < %s",lo,x,hi) end end

function BIN:select(t)
  t = t.cells and t.cells or t
  local x, lo, hi = t[self.pos], self.lo, self.hi
  return x=="?" or lo == hi and lo == x or lo <= x and x < hi end
---     ._        ._ _  
---     | |  |_|  | | | 

function NUM:new(pos,s) 
  self.pos, self.txt, self.lo, self.hi = pos or 0,s or "",1E32, -1E32
  self.n, self.mu, self.m2 = 0,0,0
  self.w = self.txt:find"-$" and -1 or 1  end

function NUM:add(x,   _,d) 
  if x ~="?" then
    self.n  = self.n + 1
    self.lo = math.min(x, self.lo)
    self.hi = math.max(x, self.hi) 
    d       = x - self.mu
    self.mu = self.mu + d/self.n
    self.m2 = self.m2 + d*(x - self.mu) end
  return x end

function NUM:mid() return self.mu end
function NUM:div() return (self.m2/(self.n - 1))^0.5 end

function NUM:norm(x,   lo,hi)
  lo,hi= self.lo, self.hi
  return x=="?" and x or hi-lo < 1E-9 and 0 or (x - lo)/(hi - lo) end 

function NUM:dist(x,y)
  if     x=="?" and y=="?" then return 1 end
  if     x=="?"            then y = self:norm(y); x = y<.5 and 1 or 0 
  elseif y=="?"            then x = self:norm(x); y = x<.5 and 1 or 0
  else x,y = self:norm(x), self:norm(y) end
  return math.abs(x - y) end
---      _   _   |   _ 
---     (_  (_)  |  _> 

function COLS:new(names,       it,num,sym,col)
  self.names, self.x, self.y, self.all = names, {},{},{}
  for pos,txt in pairs(names) do 
    col = push(self.all, (txt:find"^[A-Z]" and NUM or SYM)(pos,txt))
    if not txt:find":$" then
      if txt:find"!$" then self.klass = col end
      push(txt:find"[-+!]$" and self.y or self.x, col) end end end

---     ._   _        
---     |   (_)  \/\/ 

function ROW:new(data,t)
  self._data,self.cells, self.evaluated = data,t, false end

function ROW:__sub(other,    cols,d,inc)
  d, cols = 0, self._data.cols.x
  for _,col in pairs(cols) do
    inc = col:dist(self.cells[col.pos], other.cells[col.pos]) 
    d   = d + inc^the.p end
  return (d / #cols) ^ (1/the.p) end

function ROW:__lt(other,   s1,s2,e,y,a,b)
  y= self._data.cols.y
  s1, s2, e = 0, 0,  math.exp(1)
  for _,col in pairs(y) do
     a = col:norm(self.cells[col.pos])
     b = col:norm(other.cells[col.pos])
     s1= s1 - e^(col.w * (a - b) / #y)
     s2= s2 - e^(col.w * (b - a) / #y) end
  return s1/#y < s2/#y  end

---     ._   _          _ 
---     |   (_)  \/\/  _> 

function EGS:new() self.rows,self.cols = {},nil end

function EGS:add(t)
  if   self.cols 
  then t = push(self.rows, t.cells and t or ROW(self,t)).cells
       for _,col in pairs(self.cols.all) do col:add(t[col.pos]) end
  else self.cols = COLS(t) end
  return self end

function EGS:mid(t) return map(t or self.cols.y,function(c)return c:mid()end)end
function EGS:div(t) return map(t or self.cols.y,function(c)return c:div()end)end

function EGS:clone(rows, out)
  out=EGS():add(self.cols.names)
  for _,row in pairs(rows or {}) do out:add(row) end
  return out end

function EGS:load(file)
  for t in csv(the.file) do self:add(t) end 
  return self end

function EGS:around(r1,rows,   t)
  t={}; for _,r2 in pairs(rows or self.rows) do push(t,{row=r2, d= r1 - r2}) end
  return sort(t,lt"d") end

function EGS:far(r1,rows)
  return per(self:around(r1,rows),the.far).row end

function EGS:sway(rows,stop,rest,x,           some,y,c,best,mid)
  rows = rows or self.rows
  stop = stop or 2*the.best*#rows
  if #rows <= stop then return rows,rest end
  rest = rest or {}
  some = many(rows,the.some)
  x    = x or self:far(any(some), some)
  y    =      self:far(x,         some)
  if y < x then x,y = y,x end
  x.evaluated = true
  y.evaluated = true
  c    = x - y
  rows = map(t,function(r) return {r=r, x=((r-x)^2+c^2-(r-y)^2)/(2*c)} end) 
  best = {}
  mid  = #rows//2
  for i,rx in pairs(sort(rows,lt"x")) do push(i<=mid and best or rest, rx.r) end
  return self:sway(best,stop,rest,x)  end
---    ___  ____ _  _ ____ ____ 
---    |  \ |___ |\/| |  | [__  
---    |__/ |___ |  | |__| ___] 

local go,no,fails,ok,main={},{},0

function main(   all,b4)
  all={}; for k,_ in pairs(go) do push(all,k) end
  for _,x in pairs(the.go=="all" and sort(all) or {the.go}) do 
    b4={}; for k,v in pairs(the) do b4[k]=v end
    math.randomseed(the.seed)
    if go[x] then print(x); go[x]() end 
    for k,v in pairs(b4) do the[k]=v end end end

function ok(test,msg)
  print("", test and "PASS "or "FAIL ", msg or "")
    if not test then
      fails= fails+1
      if the.dump then assert(test,msg) end end end

function go.rogue( ok)
  ok={}; for _,k in pairs{ "_G", "_VERSION", "arg", "assert", "collectgarbage",
  "coroutine", "debug", "dofile", "error", "getmetatable", "io", "ipairs",
  "load", "loadfile", "math", "next", "os", "package", "pairs", "pcall",
  "print", "rawequal", "rawget", "rawlen", "rawset", "require", "select",
  "setmetatable", "string", "table", "tonumber", "tostring", "type", "utf8",
  "warn", "xpcall"} do ok[k]=true end
  for k,v in pairs(_ENV) do if not ok[k] then print("?",k, type(v)) end end end

function go.the()  oo(the) end
function go.eg( n,out)   
  out =true
  n=0; for row in csv(the.file) do 
         n=n+1; out=out and #row==8
                if n>1 then out=out and type(row[1])=="number" end end
  ok(out and n==399); end

function go.rows( egs) 
  egs=EGS():load(the.file) 
  map(egs.cols.x,oo); print(""); 
  map(egs.cols.y,oo) end

function go.dist( egs,    a,b,c,out)
  egs  = EGS():load(the.file)
  out = true
  for i=1,100 do
    a,b,c = any(egs.rows), any(egs.rows), any(egs.rows)
    out   = out and (b -a)==(a-b) and (a-a)==0 and ((a-b)+ (b-c) >= (a-c)) end 
  ok(out,"dist") end

function go.sort(    egs,rows,n)
  egs  = sort(EGS():load(the.file))
  rows= sort(egs.rows)
  n = .05*#rows//1
  print("what", o(map(egs.cols.y,function(c) return c.txt end)))
  print("all",  o(rnds(egs:mid())))
  print("best", o(rnds(egs:clone(slice(rows, 1, n)):mid())))
  print("rest", o(rnds(egs:clone(slice(rows, n+1 )):mid())))
  end

function go.far(  egs)
  egs = EGS():load(the.file)
  print(egs:far(egs.rows[1])) end
 
function go.sway(  egs,best,rest)
  egs = EGS():load(the.file)
  best,rest = egs:sway() 
  end
 

-------------------------------------------------------------------------------
---    ____ ___ ____ ____ ___ 
---    [__   |  |__| |__/  |  
---    ___]  |  |  | |  \  |  

for k,v in pairs(the) do the[k] = string2thing(v) end 
if the.help then print(help) else main() end
go.rogue()
os.exit(fails) 
