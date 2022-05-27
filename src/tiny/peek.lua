local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local atom,big,bins,cli,csv,fmt,gt,is,lt,map,o,oo
local per,push,rand,rnd,sort,splice,the,tothing
local EGS, NUM, ROW, ROWS, SOME, SYM
local help=[[  
PEEK: landscape analysis 
(c) 2022 Tim Menzies, timm@ieee.org, BSD2 license 
"I think the highest and lowest points are the important ones. 
 Anything else is just... in between." ~Jim Morrison

INSTALL: requires: lua 5.4+
         download: peek.lua
         test    : lua peek.lua -h

USAGE: lua peek.lua [OPTIONS]
                                   defaults
                                   --------
  --Seed  -S  random number seed   = 10019
  --bins  -b  number of bins       = 16
  --min   -m  min size pass1       = .5
  --p     -p  distance coefficient = 1
  --some  -s  sample size          = 512

OPTIONS (other):
  --file  -f  csv file with data   = ../../etc/data/auto93.csv
  --go    -g  start up action      = nothing
  --help  -h  show help            = false]]
--------------------------------------------------------------------------------
function atom(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x  end
     
function cli(d)
  for key,x in pairs(d) do
    x = tostring(x)
    for n,flag in ipairs(arg) do 
      if flag==("-"..key:sub(1,1)) or flag==("--"..key) then 
        x = x=="false" and"true" or x=="true" and"false" or arg[n+1] end end
    d[key] = atom(x) end  end

local the={}
help:gsub(" [-][-]([^%s]+)[^\n]*%s([^%s]+)",function(k,x) the[k] = atom(x) end)
help=help:gsub("[%u][%u%d]+", "\27[31m%1\27[0m")           -- highlight capitals
         :gsub("\"[^\"]+\"", "\27[32m%1\27[0m")            -- highlight strings  
         :gsub("(%s)([-][-]?[^%s]+)(%s)","%1\27[33m%2\27[0m%3")--highlight flags
 
cli(the)
if the.help then os.exit(print(help)) end
math.randomseed(the.Seed)
--------------------------------------------------------------------------------
big = math.huge
rand= math.random
fmt = string.format
function rnd(n, p) local m=10^(p or 0); return math.floor(n*m+0.5)/m  end

function lt(x) return function(a,b) return a[x] < b[x] end end
function gt(x) return function(a,b) return a[x] > b[x] end end
function map(t,f,  u) u={}; for k,v in pairs(t) do u[1+#u]=f(v) end return u end
function sort(t,f) table.sort(t,f); return t end
function push(t,x) t[1+#t]=x; return x end
function per(t,p, i) i=p*#t//1; return t[math.max(1,math.min(#t,i))] end
function splice( t, i, j, k,    u) 
  u={}; for n=(i or 1), (j or #t), (k or 1) do u[1+#u]=t[n] end return u end

function csv(csvfile) 
  csvfile = io.input(csvfile)
  return function(s, t) 
    s=io.read()
    if not s then io.close(csvfile) else
      t={}; for x in s:gmatch("([^,]+)") do t[1+#t]=atom(x) end
      return t end end end 

function oo(x) print(o(x)); return x end
function o(t,    u)
  if #t>0 then return "{"..table.concat(map(t,tostring)," ").."}" end
  u={}; for k,v in pairs(t) do u[1+#u] = string.format(":%s %s",k,v) end
  return (t.is or "").."{"..table.concat(sort(u)," ").."}" end 

function is(name,    t,new)
  function new(kl,...) local x=setmetatable({},kl); kl.new(x,...); return x end 
  t = {__tostring=o, is=name or ""}; t.__index=t 
  return setmetatable(t, {__call=new}) end 
---------------------------------------------------------------------------------
SOME=is"SOME"
function SOME.new(i) i.all, i.ok, i.n = {}, false,0 end
function SOME.add(i,x,     a) 
  i.n, a = 1 + i.n, i.all
  if     #a     < the.some     then i.ok=false; push(a,x)  
  elseif rand() < the.some/i.n then i.ok=false; a[rand(#a)]=x end end 

function SOME.has(i) if not i.ok then sort(i.all) end;i.ok=true; return i.all end
---------------------------------------------------------------------------------
NUM=is"NUM"
function NUM.new(i,at,txt) 
  i.at,i.txt=at or 0,txt or ""; i.hi=-big;i.lo=big; i.n,i.mu=0,0 
  i.w = i.txt:find"-$" and -1 or 1 
  i.all = SOME() end

function NUM.add(i,x) 
  if x ~="?" then 
    i.all:add(x) 
    i.n     = i.n + 1
    local d = x - i.mu
    i.mu    = i.mu + d/i.n
    i.hi=math.max(x,i.hi); i.lo=math.min(x,i.lo) end end

function NUM.clone(i) return NUM(i.at,i.txt) end
function NUM.mid(i) return i.mu end
function NUM.div(i,  a) print("!!!!"); a=i.all:has(); return (per(a,.9) - per(a,.1))/2.56 end
function NUM.norm(i,x)
  return x=="?" and x or i.hi-i.lo<1E-9 and 0 or (x - i.lo)/(i.hi - i.lo) end

function NUM.bin(i,v,  b) b=(i.hi-i.lo)/the.bins;return math.floor(v/b+0.5)*b end
function NUM.bins(i,bins,lo,hi,enough)
  local lhs, rhs, all, out = SYM(), SYM(), SYM(), {}
  print""
  for j=lo,hi do 
    for x,n in pairs(bins[j].y.all) do print("==",x,n); all:add(x,n);rhs:add(x,n)end end
  local n,best,cut = rhs.n, rhs:div()
  print("best",best)
  for j=lo,hi do
    for x,n in pairs(bins[j].y.all) do lhs:add(x,n); rhs:sub(x,n) end
    print("rle", rhs.n, lhs.n, enough)
    if rhs.n >= enough and lhs.n >= enough then
      local tmp= rhs:div()*rhs.n/n + lhs:div()*lhs.n/n 
      print("tmp",tmp)
      if tmp < best*1.01 then cut,best =j,tmp end end end
  if cut 
  then i:bins(bins, lo, cut,    enough, out)
       i:bins(bins,  cut+1, hi, enough, out)
  else local hi1 = hi < #bins and bins[hi+1].lo or big
       push(out, {at=i.at, lo=bins[lo].lo, hi=hi1, y=all}) 
  end 
  out[1].lo    = -big
  out[#out].hi =  big
  return out end
--------------------------------------------------------------------------------
SYM=is"SYM"
function SYM.new(i,at,txt) 
  i.at,i.txt = at or 0,txt or ""; i.n,i.all=0,{}; i.most,i.mode=0 end
function SYM.add(i,x,inc) 
  if x~="?" then 
    inc = inc or 1
    i.n = i.n+inc
    i.all[x] = inc + (i.all[x] or 0)
    if i.all[x] > i.most then i.most,i.mode=i.all[x], x end end end

function SYM.sub(i,x,inc) 
  if x~="?" then 
    inc = inc or 1
    i.n = i.n - inc
    i.all[x] = i.all[x]  - inc end end
 
function SYM.mid(i) return i.mode end
function SYM.div(i,   e)
  e=0; 
  for k,n in pairs(i.all) do if n>0 then print(",",n); e=e-n/i.n*math.log(n/i.n,2) end end 
  return e end

function SYM.clone(i) return SYM(i.at,i.txt) end
function SYM.bin(i,x) return x end
function SYM.bins(i,bins,...) return bins end
--------------------------------------------------------------------------------
ROW=is"ROW"
function ROW.new(i,of,cells) i.of,i.cells = of,cells end
function ROW.__lt(i,j,        n,s1,s2,v1,v2)
  s1, s2, n = 0, 0, #i.of.ys
  for _,col in pairs(i.of.ys) do
    v1,v2 = col:norm(i.cells[col.at]), col:norm(j.cells[col.at])
    s1    = s1 - 2.7183^(col.w * (v1 - v2) / n)
    s2    = s2 - 2.7183^(col.w * (v2 - v1) / n) end
  return s1/n < s2/n end
--------------------------------------------------------------------------------
ROWS=is"ROWS"
function ROWS.new(i,src)
  i.all={}; i.cols=nil; i.xs={}; i.ys={}
  if type(src)=="string" then for   row in csv(  src) do i:add(row) end 
                         else for _,row in pairs(src) do i:add(row) end end end

function ROWS.clone(i,inits,   j)
  j=ROWS({ map(i.cols, function (col) return col.txt end) })
  for _,row in pairs(inits or {}) do j:add(row) end
  return j end

function ROWS.add(i,row) if i.cols then i:data(row) else i:header(row) end end

function ROWS.header(i,row,   col)
  i.cols = {}
  for at,s in pairs(row) do
    col = push(i.cols, (s:find"^[A-Z]" and NUM or SYM)(at,s))
    if not s:find":$" then
      push(s:find"[!+-]$" and i.ys or i.xs, col) end end end
  
function ROWS.data(i,row)
  row = push(i.all, row.cells and row or ROW(i,row))
  for _,col in pairs(i.cols) do col:add(row.cells[col.at]) end end

function ROWS.mid(i,    p,t) 
  t={}; for _,col in pairs(i.ys) do t[col.txt]=rnd(col:mid(),p or 3) end
  return t end

function ROWS.bins(i,bests,rests)
  print(#bests, #rests)
  local function bins1(col)
    local tmp, bins = {}, {}
    for klass,rows in pairs{bests,rests} do
      for n,row in pairs(rows) do
        local x = row.cells[col.at]
        if x ~= "?" then
          x = col:bin(x)
          tmp[x] = tmp[x] or push(bins, {at=col.at, lo=x, hi=x, y=SYM()})
          tmp[x].y:add(klass)
        end 
      end 
    end
    return col:bins(sort(bins,lt"lo"), 1, #bins, (#bests+#rests)^the.min)  
  end --------------------------------------------------
  local out={}
  for _,col in pairs(i.xs) do 
    print("===",col.at)
    for _,bin in pairs(bins1(col)) do push(out,bin) end end
  for k,v in pairs(out) do print(k,o(v)) end
  return out end
--------------------------------------------------------------------------------
local rows = ROWS(the.file)
sort(rows.all)
local n=#rows.all
local m=n^the.min // 1
local bests = splice(rows.all,1,m)
local rests = splice(rows.all,n - m)
rows:bins(bests,rests)
--
for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end--[5]
