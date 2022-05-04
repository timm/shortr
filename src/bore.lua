local help = [[

BORE: best or rest. u show me a good loser and i'll show u a loser.
(c) 2022, Tim Menzies <timm@ieee.org> opensource.org/licenses/Fair

USAGE:
  alias bore="lua bore.lua "
  bore [OPTIONS]

OPTIONS:
  --bins    -b  max bins                 = 16

OPTIONS (other):
  --seed    -s  random number seed       = 10019
  --file    -f  where to find data       = ../etc/data/auto93.csv
  --dump    -d  dump stack+exit on error = false
  --help    -h  show help                = false
  --go      -g  start up action          = nothing
]] 

local function thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x end

local the={}
help:gsub("\n  ([-][-]([^%s]+))[%s]+(-[^%s]+)[^\n]*%s([^%s]+)",function(f1,k,f2,x)
  for n,flag in ipairs(arg) do if flag==f1 or flag==f2 then
    x = x=="false" and"true" or x=="true" and"false" or arg[n+1] end end 
  the[k] = thing(x) end) 
--------------------------------------------------------------------------------
local atom,csv,map,merge,o,oo,obj,ok,on,patch,per,push,rows,sort
local _,GO,BIN,NUM,SYM,COLS,ROW,EGS
local R,big,fmt

big = math.huge
R   = math.random
fmt = string.format

function push(t,x)    t[1+#t]=x;       return x end
function sort(t,f)    table.sort(t,f); return t end
function map(t,f, u)  u={}; for k,v in pairs(t) do u[1+#u]=f(v) end;return u end
function per(t,p, i)  i=(p or.5)*#t//1; return t[math.max(1,math.min(#t,i))] end

function on(i,defaults,new)
  for k,v in pairs(defaults) do i[k] = v end
  for k,v in pairs(new or{}) do assert(i[k]~=nil,"bad slot:"..k); i[k]=v end end

function csv(src)
  src = io.input(src)
  return function(line, row) 
    line=io.read()
    if not line then io.close(src) else
      row={}; for x in line:gmatch("([^,]+)") do row[1+#row]=thing(x) end
      return row end end end 

function oo(t) print(o(t)) end
function o(t,    u)
  if #t>0 then return "{"..table.concat(map(t,tostring)," ").."}" else
    u={}; for k,v in pairs(t) do u[1+#u] = fmt(":%s %s",k,v) end
    return (t.is or "").."{"..table.concat(sort(u)," ").."}" end end

function obj(name,    t,new)
  function new(kl,...) 
    local x=setmetatable({},kl); kl.new(x,...); return x end 
  t = {__tostring=o, is=name or ""}; t.__index=t
  _ = t
  return setmetatable(t, {__call=new}) end
--------------------------------------------------------------------------------
BIN=obj"BIN"
function _.new(i,t) on(i,{at=0, txt="", lo=big, hi= -big, ys={}},t) end
function _.of(i,x)  return i.ys.has[x] or 0 end

function _.select(i,t,     x)
  t = t.cells and t.cells or t
  x = t[i.pos]
  return x=="?" or i.lo == i.hi and i.lo == x or i.lo <= x and x < i.hi end

function _.__tostring(i)
  local x, lo, hi = i.txt, i.lo, i.hi
  if     lo ==  hi  then return fmt("%s == %s",x, lo)  
  elseif hi ==  big then return fmt("%s >= %s",x, lo)  
  elseif lo == -big then return fmt("%s < %s", x, hi)  
  else                   return fmt("%s <= %s < %s",lo,x,hi) end end

function _.merged(i,j,    k)
  k = i.ys:merged(j.ys)
  if k then return BIN{at=i.at, txt=i.txt, lo=i.lo, hi=j.hi, ys=k} end end
--------------------------------------------------------------------------------
SYM=obj"SYM"
function _.new(i,t)    on(i,{at=0, txt="", has={}, bins={}},t) end
function _.add(i,x,n)  if x~="?" then i.has[x]=(n or 1)+(i.has[x] or 0) end end
function _.addy(i,x,y) 
  if x~="?" then 
    i.bins[x] = i.bins[x] or BIN{at=i.at, txt=i.txt, lo=x, hi=x, ys=SYM()} 
    i.bins[x].ys:add(y) end end

function _.mid(i,   m,x)
  m=0; for y,n in pairs(i.has) do if n>m then m,x=n,y end end; return x end

function _.div(i,   n,e)
  n=0; for k,m in pairs(i.has) do n = n + m end 
  e=0; for k,m in pairs(i.has) do e = e - m/n*math.log(m/n,2) end 
  return e,n end

function _.merge(i,j,    k)
  k=SYM{at=i.at, txt=i.txt}
  for x,n in pairs(i.has) do k:add(x,n) end
  for x,n in pairs(j.has) do k:add(x,n) end
  return k end

function _.merged(i,j,  k)
  k = i:merge(j)
  div1, n1 = i:div()
  div2, n2 = j:div()
  if k:div() < (div1*n1 + div2*n2) / (n1+n2) then return k end end
--------------------------------------------------------------------------------
NUM=obj"NUM"
function _.new(i,t) 
  on(i,{at=0,txt="",lo= big,hi= -big, all={}, bins={}},t) 
  i.w = i.txt:find"-$" and -1 or 1 end

function _.norm(i,x) return x=="?" and x or (x-i.lo)/(i.hi - i.lo) end

function _.add(i,x)
  if x=="?" then return x end
  i.ok = nil
  push(i.all,x)
  if x >i.hi then i.hi=x elseif x<i.lo then i.lo=x end end

function _.addy(i,x,y,   gap)
  if x=="?" then return x end
  gap = (i.hi - i.lo)/the.bins
  x   = (x    - i.lo)//gap
  i.bins[x] = i.bins[x] or BIN{at=i.at, txt=i.txt, lo=x, hi=x+gap, ys=SYM()} 
  i.bins[x].ys:add(y) end

function _.mid(i) 
  i.all = i.ok and i.all or sort(i.all); i.ok=true
  return per(i.all, .5) end

function _.div(i) 
  i.all = i.ok and i.all or sort(i.all); i.ok=true
  return (per(i.all, .9) - per(i.all, .1)) / 2.56 end

function merge(b4,        a,b,c,j,n,tmp)
  j, n, tmp = 1, #b4, {}
  while j<=n do
    a, b = b4[j], b4[j+1]
    if b then c = a:merged(b)
              if c then a, j = c, j+1 end end
    tmp[#tmp+1] = a
    j = j+1 end
  return #tmp==#b4 and tmp or merge(tmp) end

function patch(t)
  for j=2,#t do  t[j].lo = t[j-1].hi end
  t[1].lo  = -big
  t[#t].hi =  big
  return t end
--------------------------------------------------------------------------------
ROW=obj"ROW"
function _.new(i,t) on(i,{cells={},data={}},t) end

function _.__lt(i,j,     s1,s2,e,y,a,b)
  y = i.data.cols.y
  s1, s2, e = 0, 0,  math.exp(1)
  for _,col in pairs(y) do
     a = col:norm(i.cells[col.at])
     b = col:norm(j.cells[col.at])
     s1= s1 - e^(col.w * (a - b) / #y)
     s2= s2 - e^(col.w * (b - a) / #y) end
  return s1/#y < s2/#y  end
--------------------------------------------------------------------------------
COLS=obj"COLS"
function _.new(i,t,     col)
  on(i, {all={}, x={}, y={}, names={}},t)
  for at,txt in pairs(i.names) do
    col = push(i.all, (txt:find"^[A-Z]" and NUM or SYM){at=at, txt=txt})
    if not txt:find":$" then
      push(txt:find"[-+!]$" and i.y or i.x, col) end end end 
--------------------------------------------------------------------------------
EGS=obj"EGS"
function _.new(i)       i.rows,i.cols= {},nil end
function _.file(i,file) for row in csv(file) do  i:add(row) end; return i end
function _.add(i,row)
  if   i.cols 
  then row = push(i.rows, row.cells and row or ROW{data=i, cells=row}).cells
       for k,col in pairs(i.cols.all) do col:add(row[col.at]) end
  else i.cols = COLS{names=row} end end

function _.mid(cs) return map(cs or i.cols.y,function(c) return c:mid() end) end
function _.div(cs) return map(cs or i.cols.y,function(c) return c:div() end) end
--------------------------------------------------------------------------------
GO=obj"GO"
function ok(test,msg)
  print("", test and "PASS "or "FAIL ", msg or "")
  if not test then
    GO.fails= GO.fails+1
    if the.dump then assert(test,msg) end end end

function _.new(i,todo,    b4,go)
  b4={}; for k,v in pairs(the) do b4[k]=v end
  go={}; for k,_ in pairs(GO) do
           if k~="new" and type(GO[k])=="function" then go[1+#go]=k end end
  GO.fails = 0
  for _,x in pairs(todo=="all" and sort(go) or {todo}) do
    for k,v in pairs(b4) do the[k]=v end 
    math.randomseed(the.seed)
    if GO[x] then print(x); GO[x]() end end 
  GO.rogue()
  os.exit(fails) end

function GO.rogue( t)
  t={}; for _,k in pairs{ "_G", "_VERSION", "arg", "assert", "collectgarbage",
  "coroutine", "debug", "dofile", "error", "getmetatable", "io", "ipairs",
  "load", "loadfile", "math", "next", "os", "package", "pairs", "pcall",
  "print", "rawequal", "rawget", "rawlen", "rawset", "require", "select",
  "setmetatable", "string", "table", "tonumber", "tostring", "type", "utf8",
  "warn", "xpcall"} do t[k]=k end
  for k,v in pairs(_ENV) do if not t[k] then print("?",k, type(v)) end end end

function GO.cols()
  oo(COLS{names={"Cyldrs", "Acc+"}}) end

function GO.egs(  egs,a)
  egs = EGS():file(the.file)
  a=egs.rows
  sort(a)
  for j=1,5 do 
    for _,col in pairs(egs.cols.x) do col:addy(a[j].cells[col.at],true)  end end
  for j=#a-5,#a do 
    for _,col in pairs(egs.cols.x) do col:addy(a[j].cells[col.at],false) end end
  end

--------------------------------------------------------------------------------
if   the.help 
then help=help:gsub("%u%u+","\27[34m%1\27[0m"); print(help)
else GO(the.go) end

