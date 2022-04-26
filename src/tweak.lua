#!/usr/bin/env lua
-- vim: filetype=lua ts=2 sw=2 et:
--     __                                     __         
--    /\ \__                                 /\ \        
--    \ \ ,_\   __  __  __     __      _     \ \ \/'\    
--     \ \ \/  /\ \/\ \/\ \  /'__`\  /'__`\   \ \ , <    
--      \ \ \_ \ \ \_/ \_/ \/\  __/ /\ \L\.\_  \ \ \\`\  
--       \ \__\ \ \___x___/'\ \____\\ \__/.\_\  \ \_\ \_\
--        \/__/  \/__//__/   \/____/ \/__/\/_/   \/_/\/_/

local the, help= {}, [[
tweak: try three weak leaners for multi-objective optimization

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
  --todo    -t  start up action             = nothing
]]

local any,cells,coerce,csv,fmt,goalp,lessp,lt,many,map,median,mode
local nump,oo,o,obj,per,push,r,rnd,rnds,sort,slice,stats,string2thing

help:gsub("\n  ([-][-]([^%s]+))[%s]+(-[^%s]+)[^\n]*%s([^%s]+)",function(f1,k,f2,x)
  for n,flag in ipairs(arg) do if flag==f1 or flag==f2 then
    x = x=="false" and "true" or x=="true" and "false" or arg[n+1] end end 
  the[k] = x end) -- not ready yet. full of strings that need `string2thing'

-- (c) 2022, Tim Menzies,  opensource.org/licenses/Fair
-- Usage of the works is permitted provided that this instrument is 
-- retained with the works, so that any entity that uses the works is 
-- notified of this instrument.  DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
---    _  _ ___ _ _    ____ 
---    |  |  |  | |    [__  
---    |__|  |  | |___ ___] 

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

r=math.random
function any(a, i)    i=r()*#a//1; i=math.max(1,math.min(i,#a)); return a[i] end
function lt(x)        return function(a,b) return a[x] < b[x] end end
function many(a,n, u) u={}; for j=1,n do push(u,any(a)) end;return u end
function map(t,f, u)  u={}; for _,v in pairs(t) do u[1+#u]=f(v) end;return u end
function per(t,p)     return t[ ((p or .5)*#t) // 1 ] end 
function push(t,x)    t[1+#t]=x; return x end
function sort(t,f)    table.sort(t,f) return t end
function slice(t,i,j,   u) 
  u={}; for k=(i or 1), (j or #t) do u[1+#u] = t[k] end return u end

---     ._   ._  o  ._   _|_ 
---     |_)  |   |  | |   |_ 
---     |                    
fmt=string.format
function oo(t) print(o(t)) end
function o(t,    u,one,pub,sorted)
  sorted = #t>0 -- true when array's indexes are 1,2...#t
  one= function(k,v) return sorted and tostring(v) or fmt(":%s %s",k,v) end
  pub= function(k) return tostring(k):sub(1,1) ~= "_" end
  u={}; for k,v in pairs(t) do if pub(k) then u[1+#u] = one(k,v) end end
  return (t.is or "").."{"..table.concat(sorted and u or sort(u)," ").."}" end

function rnds(t,f) return map(t, function(x) return rnd(x,f) end) end
function rnd(x,f) 
  return fmt(type(x)=="number" and (x~=x//1 and f or the.rnd) or"%s",x) end

---      _   |_    o   _    _  _|_   _ 
---     (_)  |_)   |  (/_  (_   |_  _> 
---               _|                   
function obj(name,    t,new,str)
  function new(kl,...) local x=setmetatable({},kl); kl.new(x,...); return x end 
  t = {__tostring=o, is=name or ""}; t.__index=t
  return setmetatable(t, {__call=new}) end

---      _.        _   ._     
---     (_|  |_|  (/_  |   \/ 
---       |                /  
function inc(t,x,n) t[x] = (t[x] or 0) + (n or 1); return t[x] end
function dec(t,x)   return inc(t,x,(n or -1)) end

function nonparametric(t,  n,e,p,u) 
  n,u=0,{}
  for _,m in pairs(t) do 
    n=n+m 
    u[x] = 1+ (u[x] or 0)
      if u[x] > most then most,mode = u[x], x end end
  e=0; for _,m in pairs(t) do p=m/n; e = e -  p*math.log(p,2) end
  return mode,e end

XXXX is a mess. make this all select from num,sym
function cells(i,rows,    x,t)
  t={}; for _,r in pairs(rows) do x=r.cells[i]; if x~="?" then t[1+#t]=x end end
  return t end

function div(t,   t)
  u={}; for _,x in pairs(t) do inc(u,x) end; return distribution(u) end

function mid(t) t=sort(t); return per(t,.5), (per(t,.9)-per(t,.1))/2.56 end

function stats(cols,t,lo,hi,    m,d,ms,ds)
  lo,hi = lo or 1, hi or #t
  ms,ds={},{};for _,c in pairs(cols) do m,d=c:mid(t); push(ms,m); push(ds,d) end
  return ms,ds end

---    ____ ___   _ ____ ____ ___ ____ 
---    |  | |__]  | |___ |     |  [__  
---    |__| |__] _| |___ |___  |  ___] 

---     |_   o  ._  
---     |_)  |  | | 

local Bin=obj"Bin"
function Bin:new(t) 
  self.pos, self.txt, self.n, self.has = t.pos, t.txt, t.n, {}
  self.lo, self.hi, self.seen = t.lo, t.hi, t.seen or {} end

function Bin:__tostring()
  local x,lo,hi,big = self.txt, self.lo, self.hi, math.huge
  if     lo ==  hi  then return fmt("%s == %s",x, lo)  
  elseif hi ==  big then return fmt("%s >= %s",x, lo)  
  elseif lo == -big then return fmt("%s < %s", x, hi)  
  else                   return fmt("%s <= %s < %s",lo,x,hi) end end

function Bin:select(t)
  t = t.cells and t.cells or t
  local x, lo, hi = t[self.pos], self.lo, self.hi
  return x=="?" or lo == hi and lo == x or lo <= x and x < hi end


---      _      ._ _  
---     _>  \/  | | | 
---         /         

local Sym=obj"Sym"
function Sym:new(pos,s) 
  self.pos, self.txt= pos or 0,s or "" 
  self.has, self.most, self.mode = {},0, nil end

function Sym:add(x)    return x end   
function Sym:dist(x,y) return x=="?" and y=="?" and 1 or x==y and 0 or 1 end
function Sym:mid(rows) return mode(cells(self.pos,rows)) end

function Sym:bins(rows,     x,n,out,has,tmp,inc)
  n,out,tmp = 0,{},{}
  function inc(x) n=n+1; return n end
  function has(x) tmp[x]=tmp[x] or Bin({txt=self.txt,  pos=self.pos, n=inc(x),
                                        lo=x ,hi=x, seen={}}) end
  for _,r in pairs(rows) do 
    x = r.cells[self.pos]; has(x); push(tmp[x].seen,r.klass) end
  for _,x in pairs(tmp) do push(out, x) end
  return out end

---     ._        ._ _  
---     | |  |_|  | | | 

local Num=obj"Num"
function Num:new(pos,s) 
  self.pos, self.txt, self.lo, self.hi = pos or 0,s or "",1E32, -1E32
  self.w = self.txt:find"-$" and -1 or 1  end

function Num:add(x) 
  if x=="?" then return x end
  self.lo = math.min(x,self.lo)
  self.hi = math.max(x,self.hi) end

function Num:norm(x,   lo,hi)
  lo,hi= self.lo, self.hi
  return x=="?" and x or hi-lo < 1E-9 and 0 or (x - lo)/(hi - lo) end 

function Num:dist(x,y)
  if     x=="?" and y=="?" then return 1 end
  if     x=="?"            then y = self:norm(y); x = y<.5 and 1 or 0 
  elseif y=="?"            then x = self:norm(x); y = x<.5 and 1 or 0
  else x,y = self:norm(x), self:norm(y) end
  return math.abs(x - y) end

function Num:mid(rows) return median(cells(self.pos,rows)) end
 
---      _   _   |   _ 
---     (_  (_)  |  _> 

local Cols=obj"Cols"
function Cols:new(names,       it,num,sym,col)
  self.x, self.y, self.all = {},{},{}
  for pos,txt in pairs(names) do 
    col = push(self.all, (txt:find"^[A-Z]" and Num or Sym)(pos,txt))
    if not txt:find":$" then
      if txt:find"!$" then self.klass = col end
      push(txt:find"[-+!]$" and self.y or self.x, col) end end end

function Cols:add(t) 
  for _,col in pairs(self.all) do col:add(t[col.pos]) end; return t end 

---     ._   _        
---     |   (_)  \/\/ 

local Row=obj"Row"
function Row:new(data,t)
  self._data,self.cells, self.evaluated = data,t, false end

function Row:__sub(other,    cols,d,inc)
  d, cols = 0, self._data.cols.x
  for _,col in pairs(cols) do
    inc = col:dist(self.cells[col.pos], other.cells[col.pos]) 
    d   = d + inc^the.p end
  return (d / #cols) ^ (1/the.p) end

function Row:__lt(other,   s1,s2,e,y,a,b)
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

local Egs=obj"Egs"
function Egs:new() self.rows,self.cols = {},nil end

function Egs:add(t)
  if   self.cols 
  then push(self.rows, Row(self, self.cols:add(t))) 
  else self.cols = Cols(t) end end
      
function Egs:load(file)
  for t in csv(the.file) do self:add(t) end 
  return self end

function Egs:around(r1,rows,   t)
  t={}; for _,r2 in pairs(rows or self.rows) do push(t,{row=r2, d= r1 - r2}) end
  return sort(t,lt"d") end

function Egs:far(r1,rows)
  return per(self:around(r1,rows),the.far).row end

function Egs:sway(rows,stop,           sway)
  function sway(t,stop,rest,x,    some,y,c,t1,t2)
    if #t <= stop then return t,rest end
    some = many(t,the.some)
    x    = x or self:far(any(some), some)
    y    =      self:far(x,         some)
    if y < x then x,y = y,x end
    x.evaluated = true
    y.evaluated = true
    c  = x - y
    t1 = map(t,function(r) return {row=r, x=((r-x)^2+c^2-(r-y)^2)/(2*c)} end) 
    t2 = {}
    for i,z in pairs(sort(t1,lt"x")) do push(i<#t1//2 and t2 or rest, z.row) end
    return sway(t2, stop,rest) 
  end --------------------
  rows = rows or self.rows
  return sway(rows, stop or 2*the.best*#rows,{}) end

---    ___  ____ _  _ ____ ____ 
---    |  \ |___ |\/| |  | [__  
---    |__/ |___ |  | |__| ___] 

local go,no,fails,ok,main={},{},0

function main(   all,b4)
  all={}; for k,_ in pairs(go) do push(all,k) end
  for _,x in pairs(the.todo=="all" and sort(all) or {the.todo}) do 
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

function go.eg()   for row in csv(the.file) do oo(row) end end

function go.rows() oo(Egs():load(the.file).cols.x) end

function go.dist( egs,    a,b,c,out)
  egs  = Egs():load(the.file)
  out = true
  for i=1,100 do
    a,b,c = any(egs.rows), any(egs.rows), any(egs.rows)
    out   = out and (b -a)==(a-b) and (a-a)==0 and ((a-b)+ (b-c) >= (a-c)) end 
  ok(out,"dist") end

function go.sort(    egs,rows)
  egs  = sort(Egs():load(the.file))
  rows= sort(egs.rows)
  oo(stats(egs.cols.y, rows))
  oo(stats(egs.cols.y, slice(rows, 1, 20)))
  oo(stats(egs.cols.y, slice(rows, #rows - 20))) end

function go.far(  egs)
  egs = Egs():load(the.file)
  print(egs:far(egs.rows[1])) end
 
function go.sway(  egs,best,rest)
  egs = Egs():load(the.file)
  best,rest = egs:sway() 
  print("all", o(stats(egs.cols.y, egs.rows)))
  print("best",o(stats(egs.cols.y, best)))
  print("rest",o(stats(egs.cols.y, rest)))
  end
 

-------------------------------------------------------------------------------
---    ____ ___ ____ ____ ___ 
---    [__   |  |__| |__/  |  
---    ___]  |  |  | |  \  |  

for k,v in pairs(the) do the[k] = string2thing(v) end 
if the.help then print(help) else main() end
go.rogue()
os.exit(fails) 
