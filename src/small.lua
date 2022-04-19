local b4={}; for k,v in pairs(_ENV) do b4[k]=v end
local coerce, csv,lt,obj,push,settings,o,oo,sort,the
local the,help={},[[ 
brknbad: explore the world better, explore the world for good.
(c) 2022, Tim Menzies

     .-------.  
     | Ba    | Bad <----.  planning= (better - bad)
     |    56 |          |  monitor = (bad - better)
     .-------.------.   |  
             | Be   |   v  
             |    4 | Better  
             .------.  

USAGE:
  ./bnb [OPTIONS]

OPTIONS:
  -K       -K  manage low class counts     = 1
  -M       -M  manage low evidence counts  = 2
  -best    -B  best set                    = .5
  -bins    -b  max. number of bins         = 16
  -cohen   -c  cohen                       = .35
  -dump    -d  dump stack+exit on error    = false
  -far     -F  how far to go for far       = .9
  -file    -f  file name                   = ../etc/data/auto93.csv
  -goal    -g  goal                        = recurrence-events
  -help    -h  show help                   = false
  -leaves  -l  number of items in leaves   = .5
  -p       -p  coefficient on distance     = 2
  -rest    -R  rest is -R*best             = 4
  -seed    -S  seed                        = 10019
  -some    -s  sample size for distances   = 512
  -todo    -t  start up action             = nothing
  -wait    -w  wait                        = 10]]
--------------------------------------------------------------------------------     
function coerce(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x end

help:gsub("\n  ([-]([^%s]+))[%s]+(-[^%s]+)[^\n]*%s([^%s]+)",
  function(long,key,short,x)
    for n,flag in ipairs(arg) do 
      if flag==short or flag==long then
        x = x=="false" and "true" or x=="true" and "false" or arg[n+1] end end 
    the[key] = coerce(x) end)

function o(t,u)
  u={}; 
  for k,v in pairs(t) do 
    u[1+#u] = #t>1 and tostring(v) or string.format(":%s %s",k,v) end
  table.sort(u)
  return (t.is or "").."{"..table.concat(u," ").."}" end

function obj(name,    t,new)
  function new(kl,...) 
    local obj=setmetatable({},kl); kl.new(obj,...); return obj end 
  t={__tostring=o, is=name or ""}; t.__index=t
  return setmetatable(t, {__call=new}) end
--------------------------------------------------------------------------------     
local Num=obj"Num"
function Num:new(at,txt) 
   oo{new=true,at=at,txt=txt}
   self.at  = at or 0
   self.txt = txt or ""
   self.n, self.mu, self.m2 = 0,0,0
   self.w   = self.txt:find"-$" and -1 or 1
   self.lo, self.hi = math.huge, -math.huge end

function Num:add(x,        d)
  if x ~="?" then
    self.n  = self.n + 1
    self.lo = math.min(x, self.lo)
    self.hi = math.max(x, self.hi) 
    d       = x - self.mu
    self.mu = self.mu + d/self.n
    self.m2 = self.m2 + d*(x - self.mu) end
  return x end

function Num:mid() return x.mu end
function Num:div() return (self.m2/(self.n - 1))^0.5 end

function Num:norm(x,   lo,hi)
  lo,hi= self.lo, self.hi
  return x=="?" and x or hi-lo < 1E-9 and 0 or (x - lo)/(hi - lo) end 
--------------------------------------------------------------------------------     
local Sym=obj"Sym"
function Sym:new(at,txt) 
   oo{at=at,txt=txt}
   self.at  = at or 0
   self.txt = txt or ""
   self.n   = 0
   self.has, self.mode, self.most = {},nil,0 end

function Sym:add(x,inc)
  if x ~= "?" then
    inc = inc or 1
    self.n = self.n + inc
    self.has[x] = inc + (self.has[x] or 0)
    if self.has[x] > self.most then self.most,self.mode = self.has[x],x end end
  return x end

function Sym:mid() return self.mode end
function Sym:div(  e)
  e=0; for _,v in pairs(t) do e=e-v/self.n*log(v/self.n,2) end; return e end
--------------------------------------------------------------------------------     
local Cols=obj"Cols"
function Cols:new(names,    col)
  self.names, self.all, self.x, self.y, self.klass = names, {}, {}, {}, nil
  for at,txt in pairs(names) do
    col = push(self.all, (txt:find"^[A-Z]" and Num or Sym)(at,txt))
    if not txt:find":$"  then
      if txt:find"!$" then self.klass=col end 
      col.indep = not txt:find"[-+!]$"
      push(col.indep and self.x or self.y, col) end end  end

function Cols:add(row)
  for _,col in pairs(self.all) do col:add(row[col.at]) end
  return row end
--------------------------------------------------------------------------------     
local Egs=obj"Egs"
function Egs:new() self.rows,self.cols = {}, nil end

function Egs:load(file) 
  for row in csv(file) do oo(row); self:add(row) end; return self end

function Egs:add(t)
  if self.cols then push(self.rows, self.cols:add(t)) else self.cols=Cols(t) end
  return t end

function Egs:better(row1,row2)
  local s1, s2, n, e = 0, 0, #self.cols.y, math.exp(1)
  for _,col in pairs(self.cols.y) do
    print(col)
    local a = col:norm(row1[col.at])
    local b = col:norm(row2[col.at])
    s1      = s1 - e^(col.w * (a - b) / n)
    s2      = s2 - e^(col.w * (b - a) / n) end
  return s1 / n < s2 / n  end

function Egs:betters()
  return sort(self.rows, function(a,b) return self:better(a,b) end)  end
--------------------------------------------------------------------------------     
function csv(src,      cells)
  function cells(s,  t) 
    t={}; for y in s:gmatch("([^,]+)") do t[1+#t]=coerce(y) end; return t end
  src = io.input(src)
  return function(x) x=io.read()
    if x then return cells(x) else io.close(src) end end end 

function lt(x) return function(t,u) return t[x] < u[x] end end

function oo(t) print(o(t)) end

function push(t,x) t[1+#t]=x; return x end

function sort(t,f) table.sort(t,type(f)=="string" and lt(f) or f); return t end
--------------------------------------------------------------------------------     
local go,no={},{}
function go.eg1(   eg)
  eg=Egs()
  print(eg)
  eg= Egs():load(the.file) end
--------------------------------------------------------------------------------     
if the.help then os.exit(print(help)) end  
if arg[1] and go[arg[1]] then go[arg[1]]() end
for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end 
