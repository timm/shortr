-- vim: ts=2 sw=2 et:
local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end
local help = [[  
LESS: best or rest multi-objective optimization.
(c) 2022 Tim Menzies, timm@ieee.org
"I think the highest and lowest points are the important ones. 
 Anything else is just...in between." ~ Jim Morrison

USAGE: lua bore.lua [OPTIONS]

OPTIONS:
  -b  --bins  max bins                 = 16
  -s  --seed  random number seed       = 10019
  -S  --some  number of nums to keep   = 256

OPTIONS (other):
  -f  --file  where to find data       = ../etc/data/auto93.csv
  -h  --help  show help                = false
  -g  --go    start up action          = nothing

Usage of the works is permitted provided that this instrument is
retained with the works, so that any entity that uses the works is
notified of this instrument. DISCLAIMER:THE WORKS ARE WITHOUT WARRANTY. ]] 

local the={}
local big,csv,eg,fmt,main,map,num,o,oo,per,push
local rand,range,sort,some,same,string2thing,sym,thes
local SOME,NUM,SYM,RANGE,EGS,COLS
--------------------------------------------------------------------------------
big=math.huge
rand=math.random
fmt=string.format

function same(x)   return x end
function push(t,x) t[1+#t]=x; return x end
function sort(t,f) table.sort(#t>0 and t or map(t,same), f); return t end
function map(t,f, u)  u={}; for k,v in pairs(t) do u[1+#u]=f(v) end;return u end

function string2thing(x)
    x = x:match"^%s*(.-)%s*$"
    if x=="true" then return true elseif x=="false" then return false end
    return math.tointeger(x) or tonumber(x) or x  end

function csv(src)
  src = io.input(src)
  return function(line, row) 
    line=io.read()
    if not line then io.close(src) else
      row={}; for x in line:gmatch("([^,]+)") do push(row,string2thing(x)) end
      return row end end end 

function oo(t) print(o(t)) end
function o(t,    u)
  if #t>0 then return "{"..table.concat(map(t,tostring)," ").."}" else
    u={}; for k,v in pairs(t) do u[1+#u] = fmt(":%s %s",k,v) end
    return (t.is or "").."{"..table.concat(sort(u)," ").."}" end end
--------------------------------------------------------------------------------
function SOME() return {all={}, ok=false, n=0} end
function some(i,x)
  if x=="?" then return x end
  i.n = 1 + i.n
  if     #i.all < the.some     then i.ok=false; push(i.all, x) 
  elseif rand() < the.some/i.n then i.ok=false; i.all[rand(#i.all)]=x end end 

function per(i,p)
  i.all = i.ok and i.all or sort(i.all); i.ok=true 
  return i.all[math.max(1, math.min(#i.all, (p or .5)*#i.all//1))] end
--------------------------------------------------------------------------------
function NUM(at,txt) return {at=0,txt="", lo=big,hi=-big, all=SOME(), nump=true,
                             w=(txt or""):find"-$" and -1 or 1} end 
function num(i,x)
  if x=="?" then return x end
  i.lo=math.min(i.lo,x)
  i.hi=math.max(i.hi,x)
  some(i.all,y) end

function mu(i,x) return per(i.all,.5) end
function sd(i,x) return (per(i.all,.9) - per(i.all,.1))/2.56 end

function norm(i,x) return (x-i.lo)/ (i.hi - i.lo + 1/big) end
--------------------------------------------------------------------------------
function SYM(at,txt) return {at=0, txt="", n=0, all={}} end
function sym(i,x) if x~="?" then i.all[x] = 1+(i.all[x] or 0) end end

function mode(i)
  m=0; for y,n in pairs(i.all) do if n>m then m,x=n,y end end; return x end

function entropy(i,   n,e)
  e=0; for k,n in pairs(i.all) do e=e-n/i.n*math.log(n/i.n,2) end ;return e end
--------------------------------------------------------------------------------
function RANGE(col,x) return {col=col, x={lo=x,hi=x}, y=SYM()} end
function range(i,x,y)
  if x=="?" then return x end
  i.lo=math.min(i.lo,x)
  i.hi=math.max(i.hi,x)
  sym(i.y, y) end
--------------------------------------------------------------------------------
function COLS(names,     head,row,i)
  i={names=t, all={}, y={}, x={}}
  for at,txt in pairs(names) do
    col = push(i.all, (txt:find"^[A-Z]" and NUM or SYM)(at, txt))
    if not txt:find":$" then
      push(txt:find"[-+!]$" and i.y or o.x, col) end end 
  return i end 
-------------------------------------------------------------------------------
function EGS(names) return {rows={}, cols=COLS(names)} end
function eg(i,row) 
  for n,col in pairs(i.cols.all) do (col.nump and num or sym)(col,row[n]) end
  push(i.row,t) end

function mid(i,cols, is) 
  cols = cols or i.cols.y
  return map(cols,function(col) return col.nump and mu(col) or mode(col) end) end
-------------------------------------------------------------------------------
local go,no={},{}

function thes(f1,f2,k,x)
  for n,flag in ipairs(arg) do if flag==f1 or flag==f2 then
    x = x=="false" and"true" or x=="true" and"false" or arg[n+1] end end 
  the[k] = string2thing(x) end 

function main(     tmp,defaults,ok,msg,fails)
  fails=0
  help:gsub("\n  ([-][^%s]+)[%s]+([-][-]([^%s]+))[^\n]*%s([^%s]+)",thes)
  if the.help then 
    print(help:gsub("%u%u+", "\27[31m%1\27[0m")
              :gsub("(%s)([-][-]?[^%s]+)(%s)","%1\27[33m%2\27[0m%3"),"")
  else
    tmp, defaults = {},{}
    for k,f in pairs(go) do if type(f)=="function" then push(tmp,k) end end 
    for k,v in pairs(the) do defaults[k]=v end
    if go[the.todo] then tmp={the.todo} end
    for _,one in pairs(sort(tmp))  do  
      for k,v in pairs(defaults) do the[k]=v end
      math.randomseed(the.seed or 10019)
      ok,msg = pcall( go[one] )
      if not ok then 
        fails=fails+1; assert(fail,fmt("[%s] %s",one,msg)) end end end 
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end 
  os.exit(fails) end

function go.some( s)
  s=SOME()
  for i=1,10^5 do some(s,rand()*100//1) end
  print(per(s)) 
  print(o(s.all), mu(s), sd(s)) end

main()
