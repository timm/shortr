local help = [[

bore: best or rest. u show me a good lower and i'll show u a loser.
(c) 2022, Tim Menzies,  timm@ieee.org, opensource.org/licenses/Fair

USAGE:
  alias bore="lua bore.lua "
  bore [OPTIONS]

OPTIONS:
  --bins    -b  max bins                 = 16

OPTIONS (other):
  --seed    -s  random number seed       = 10019
  --file    -f  where to find data       = ../etc/data/auto2.csv
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

local atom,csv,map,o,obj,ok,push,rows,with
local _,GO,BIN,NUM,SYM,COLS,ROW,EGS
local R,Big

R=math.random
Big=math.huge

function map(t,f, u)  u={}; for k,v in pairs(t) do u[1+#u]=f(v) end;return u end
function push(t,x)    t[1+#t]=x; return x end

function with(i,mew,defaults)
  for k,v in pairs(defaults) do i[k] = v end
  for k,v in pairs(new) do assert(i[k]~=nil,"missing key "..k); i[k] = v end end

function csv(f)
  f = io.input(f)
  return function(t, u)
    t=io.read()
    if not t then io.close(f) else
      u={}; for x in t:gmatch("([^,]+)") do u[1+#u]=thing(x) end
      return u end end end

function o(t,    u)
  u={}; for k,v in pairs(t) do u[1+#u] = string.format(":%s %s",k,v) end
  return (t.is or "").."{"..table.concat(sort(u)," ").."}" end

function obj(name,    t,new)
  function new(kl,...) 
    local x=setmetatable({},kl); kl.new(x,...); return x end 
  t = {__tostring=o, is=name or ""}; t.__index=t
  _ = t
  return setmetatable(t, {__call=new}) end
--------------------------------------------------------------------------------
SYM=obj"SYM"
function _.new(i,t)     with(i,t,{at=0,txt=""}) end
function _.add(i,x)     if x~="?" then i.has[x] = 1+(i.has[x] or 0) end end
function _.addxy(i,x,y) if x~="?" then i.bins[x]=y+(i.bins[x] or 0) end end

_.mid=function(i,   m,x)
  m=0; for y,n in pairs(i.has) do if n>m then m,x=y,n end end; return x end

_.div=function(i,   n,e)
  n=0; for k,m in pairs(i.has) do n = n + m end 
  e=0; for k,m in pairs(i.has) do e = e - m/n*math.log(m/n,2) end 
  return e end

--init with fields
--------------------------------------------------------------------------------
BIN=obj"BIN"
function _.new(i,t) with(i,t,{at=0,txt="",lo=Big,hi=-Big,y={}}) end
function _.of(i,x)  return i.ys.has[x] or 0 end

function _.select(i,t,     x)
  t = t.cells and t.cells or t
  x = t[i.pos]
  return x=="?" or i.lo == i.hi and i.lo == x or i.lo <= x and x < i.hi end

function _.__tostring(i)
  local x,lo,hi,big = i.txt, i.lo, i.hi, Big
  if     lo ==  hi  then return fmt("%s == %s",x, lo)  
  elseif hi ==  big then return fmt("%s >= %s",x, lo)  
  elseif lo == -big then return fmt("%s < %s", x, hi)  
  else                   return fmt("%s <= %s < %s",lo,x,hi) end end
--------------------------------------------------------------------------------
NUM=obj"NUM"
function _.new(i,t)   with(i,t,{at=0,txt="",lo=Big,hi==Big}) end
function _.norm(i,x)  return x=="?" and x or (x-i.lo)/(i.hi - i.lo) end

function _.addx(i,x)
  if x~="?" then return x end
  if x >i.hi then i.hi=x elseif x<i.lo then i.lo=x end end

function _.addxy(i,x,y)
  if x=="?" then return x end
  x = math.max(1, math.min(the.bins, the.bins*i:norm(x) // 1))
  i.bins[x] = i.bins[x] or Sym()
  i.bins[x]:add(y) end
--------------------------------------------------------------------------------
ROW=obj"ROW"
function _.new(i,t) with(i,t,{cells={},data=egs}) end
--------------------------------------------------------------------------------
COLS=obj"COLS"
function _.new(i,t,     col)
  with(i,t,{names={}})
  i.all,i.x,i.y,i.names = {},{},{},names
  for at,txt in pairs(names) do
    col = push(i.all, (txt.find"^[A-Z]+" and Num or Sym){at=at,txt=txt})
    if not txt:find":$" then
      push(txt.find"[-+!]$" and i.y or i.x,col) end end end 
--------------------------------------------------------------------------------
EGS=obj"EGS"
function _.new(i)    i.rows,i.cols= {},nil end
function _.file(i,f) for row in csv(f) do i.add(row) end end
function _.add(i,t)
  if   i.cols 
  then t = push(i.rows, t.cells and t or ROW(i,t)).cells
       for k,col in pairs(i.cols.all) do col:add(t[col.pos]) end
  else i.cols = COLS{names=t} end end
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
  "warn", "xpcall"} do t[k]=true end
  for k,v in pairs(_ENV) do if not t[k] then print("?",k, type(v)) end end end
--------------------------------------------------------------------------------
if the.help then print(help) else GO(the.go) end
