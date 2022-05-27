-- vim: filetype=lua ts=2 sw=2 et:
-- <img src="../../look/docs/heads2.png" align=left width=200>
--    
-- Semi-supervised landscape analysis.   
-- (c)2022 Tim Menzies, BSD2 license.
local help=[[  
SPY: semi-supervised landscape analysis    
(c) 2022 Tim Menzies, timm@ieee.org, BSD2 license    
"I think the highest and lowest points are the important    
 ones. Anything else is just... in between." ~Jim Morrison   
        
INSTALL: requires: lua 5.4+   
         download: spy.lua   
         test    : lua spy.lua -h   
         
USAGE: lua spy.lua [OPTIONS]   
                                              defaults   
                                              ~~~~~~~   
    --Seed  -S  random number seed            = 10019   
    --How   -H  optimize for (more,less,tabu) = more   
    --bins  -b  number of bins                = 16   
    --min   -m  min size pass1                = .5   
    --p     -p  distance coefficient          = 1   
    --some  -s  sample size                   = 512   
          
OPTIONS (other):   
    --file  -f  csv file with data            = ../../etc/data/auto93.csv   
    --go    -g  start up action               = nothing   
    --help  -h  show help                     = false]]   
--------------------------------------------------------------------------------
--
-- - asdass `asas`
-- - asdas
--
--        asdasadasd
--        asdaadadsa
--
local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local atom,big,bins,cli,csv,fmt,gt,is,lt,map,o,oo
local per,push,rand,rnd,sort,splice,the,tothing
local the = {}

function is(name,    t,new)
  function new(kl,...) local x=setmetatable({},kl); kl.new(x,...); return x end 
  t = {__tostring=o, is=name}; t.__index=t 
  return setmetatable(t, {__call=new}) end 

local EGS, NUM, RANGE      = is"EGS",is"NUM", is"RANGE" 
local ROW, ROWS, SOME, SYM = is"ROW",is"ROWS", is"SOME", is"SYM" 
---------------------------------------------------------------------------------
-- ## RANGE
function RANGE.new(i,at,txt,lo,hi,ys) 
  i.at,i.txt,i.xlo,i.xhi,i.ys=at,txt,lo,hi or lo,ys or SYM() end

function RANGE.add(i,x,y)
  if x<i.xlo then i.xlo = x end
  if x>i.xhi then i.xhi = x end
  i.ys:add(y) end

function RANGE.__tostring(i)
  local x, lo, hi = i.txt, i.xlo, i.xhi
  if     lo ==  hi  then return fmt("%s == %s",x, lo)  
  elseif hi ==  big then return fmt("%s >= %s",x, lo)  
  elseif lo == -big then return fmt("%s < %s", x, hi)  
  else                   return fmt("%s <= %s < %s",lo,x,hi) end end

function RANGE.score(i,goal,B,R,tmp)
  local how, b, r, z = {}, 0, 0, 1/big
  how.most= function(b,r) return b end
  how.more= function(b,r) return ((b<r or b+r < .05) and 0) or b^2/(b+r) end
  how.less= function(b,r) return ((r<b or b+r < .05) and 0) or r^2/(b+r) end
  how.tabu= function(b,r) return 1/(b+r) end 
  for v,n in pairs(i.ys.all) do
    if v==goal then b = b+n else r=r+n end end
  tmp= how[the.How or "more"](b/(B+z), r/(R+z))
  return tmp end

function RANGE.selects(i,row,     v)
  v = row.cells[i.at]
  return v=="?" or (i.xlo==i.xhi and i.xlo==v) or (i.xlo<=v and v<i.xhi) end
--------------------------------------------------------------------------------
-- ## SYM
function SYM.new(i,at,txt) 
  i.at,i.txt = at or 0,txt or ""; i.n,i.all=0,{}; i.most,i.mode=0 end

function SYM.add(i,x,inc) 
  if x~="?" then 
    inc = inc or 1
    i.n = i.n+inc
    i.all[x] = inc + (i.all[x] or 0)
    if i.all[x] > i.most then i.most,i.mode=i.all[x], x end end end

function SYM.bin(i,x) return x end
function SYM.bins(i,bins,...) return bins end
function SYM.mid(i,...) return i.mode end
function SYM.div(i,   e)
  e=0; for k,n in pairs(i.all) do if n>0 then e=e-n/i.n*math.log(n/i.n,2) end end 
  return e end

function SYM.sub(i,x,inc) SYM.add(i,x,-(inc or 1)) end
---------------------------------------------------------------------------------
-- ## SOME
function SOME.new(i) i.all, i.ok, i.n = {}, false,0 end
function SOME.add(i,x,     a) 
  i.n, a = 1 + i.n, i.all
  if     #a     < the.some     then i.ok=false; push(a,x)  
  elseif rand() < the.some/i.n then i.ok=false; a[rand(#a)]=x end end 

function SOME.has(i) if not i.ok then sort(i.all) end;i.ok=true; return i.all end
---------------------------------------------------------------------------------
-- ## NUM
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

function NUM.bin(i,v,  b) b=(i.hi-i.lo)/the.bins;return math.floor(v/b+0.5)*b end
function NUM.bins(i,bins,enough)
  local out={}
  local function cuts(lo,hi,       cut,lhs,rhs,all,tmp,n,best)
    lhs, rhs, all = SYM(), SYM(), SYM()
    for j=lo,hi do 
      for x,n in pairs(bins[j].ys.all) do all:add(x,n);rhs:add(x,n)end end
    n,best,cut = rhs.n, rhs:div()
    for j=lo,hi do
      for x,n in pairs(bins[j].ys.all) do lhs:add(x,n); rhs:sub(x,n) end
      if rhs.n >= enough and lhs.n >= enough then
        tmp= rhs:div()*rhs.n/n + lhs:div()*lhs.n/n 
        if tmp < best*1.01 then cut,best =j,tmp end end end
    if   cut 
    then cuts(lo, cut)
         cuts(cut+1, hi)
    else push(out, RANGE(i.at, i.txt,bins[lo].xlo, bins[hi].xhi, all)) end
  end --------------------
  cuts(1,#bins)
  for j=2,#out do out[j].xlo = out[j-1].xhi end
  out[1].xlo, out[#out].xhi = -big, big
  return out end

function NUM.clone(i)     return NUM(i.at,i.txt) end
function NUM.div(i,  a)   a=i.all:has(); return (per(a,.9) - per(a,.1))/2.56 end
function NUM.mid(i,p)     return rnd(i.mu,p or 3) end
function NUM.norm(i,x)
  return x=="?" and x or i.hi-i.lo<1E-9 and 0 or (x - i.lo)/(i.hi - i.lo) end
--------------------------------------------------------------------------------
-- ## ROW
function ROW.new(i,of,cells) i.of,i.cells = of,cells end
function ROW.__lt(i,j,        n,s1,s2,v1,v2)
  s1, s2, n = 0, 0, #i.of.ys
  for _,col in pairs(i.of.ys) do
    v1,v2 = col:norm(i.cells[col.at]), col:norm(j.cells[col.at])
    s1    = s1 - 2.7183^(col.w * (v1 - v2) / n)
    s2    = s2 - 2.7183^(col.w * (v2 - v1) / n) end
  return s1/n < s2/n end
--------------------------------------------------------------------------------
-- ## ROWS
function ROWS.new(i,src)
  i.all={}; i.cols={}; i.xs={}; i.ys={}; i.names={}
  if type(src)=="string" then for   row in csv(  src) do i:add(row) end 
                         else for _,row in pairs(src) do i:add(row) end end end

function ROWS.clone(i,inits,   j)
  j=ROWS{i.names}; for _,row in pairs(inits or {}) do j:add(row)end; return j end

function ROWS.add(i,row) 
  local function header(   col)
    i.names = row
    for at,s in pairs(row) do
      col = push(i.cols, (s:find"^[A-Z]" and NUM or SYM)(at,s))
      if not s:find":$" then
        if s:find"!$" then i.klass = col end
        push(s:find"[!+-]$" and i.ys or i.xs, col) end end 
  end -------------------------------
  if #i.cols==0 then header(row) else
    row = push(i.all, row.cells and row or ROW(i,row))
    for _,col in pairs(i.cols) do col:add(row.cells[col.at]) end  end end

function ROWS.mid(i,    p,t) 
  t={}; for _,col in pairs(i.ys) do t[col.txt]=col:mid(p) end; return t end

function ROWS.bins(i,bests,rests)
  local function bins1(col,data,         tmp,bins,x,bin, out)
    tmp, bins = {}, {}
    for klass,rows in pairs{bests,rests} do
      for n,row in pairs(rows) do
        x = row.cells[col.at]
        if x ~= "?" then
          bin  = col:bin(x)
          tmp[bin] = tmp[bin] or push(bins, RANGE(col.at,col.txt,x))
          tmp[bin]:add(x,klass) end end end
    out = col:bins(sort(bins,lt"xlo"),  (#bests+#rests)^the.min)  
    if col.is=="NUM" then out[1].xlo , out[#out].xhi = -big, big end
    return #out >1 and out or {}
  end --------
  local out={}
  for _,col in pairs(i.xs) do for _,b in pairs(bins1(col)) do push(out,b) end end
  for k,v in pairs(out) do print(k,v) end
  return out end
--------------------------------------------------------------------------------
-- ## LIB
big = math.huge
rand= math.random
fmt = string.format
     
function atom(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x  end
       
help:gsub(" [-][-]([^%s]+)[^\n]*%s([^%s]+)",function(k,x) the[k] = atom(x) end)

function cli(key,x)
  x = tostring(x)
  for n,flag in ipairs(arg) do 
    if flag==("-"..key:sub(1,1)) or flag==("--"..key) then 
      x = x=="false" and"true" or x=="true" and"false" or arg[n+1] end end
  return atom(x) end  
   
function csv(csvfile) 
  csvfile = io.input(csvfile)
  return function(s, t) 
    s=io.read()
    if not s then io.close(csvfile) else
      t={}; for x in s:gmatch("([^,]+)") do t[1+#t]=atom(x) end
      return t end end end 
   
function going(settings,funs,       defaults)
  defaults={}; for k,v in pairs(settings) do defaults[k]=v end 
  return defaults, (funs[settings.go]  and {settings.go} or sort(
     map(funs,function(x)if type(funs[x])=="function" then return x end end)))end 

function goes(fun,defaults,settings,      status)
  for k,v in pairs(defaults) do settings[k]=v end 
  math.randomseed(settings.seed or 10019)
  io.stderr:write(".")
  status = fun() 
  if status ~= true then print("-- Error",one,status); fails = fails + 1 end end       

function gt(x)        return function(a,b) return a[x] > b[x] end end
function lt(x)        return function(a,b) return a[x] < b[x] end end
function map(t,f,  u) u={}; for k,v in pairs(t) do u[1+#u]=f(v) end return u end
function oo(x)        print(o(x)); return x end
function o(t,    u)
  if #t>0 then return "{"..table.concat(map(t,tostring)," ").."}" end
  u={}; for k,v in pairs(t) do u[1+#u] = string.format(":%s %s",k,v) end
  return (t.is or "").."{"..table.concat(sort(u)," ").."}" end 
   
function per(t,p)    p=p*#t//1; return t[math.max(1,math.min(#t,p))] end
function push(t,x)   t[1+#t]=x; return x end
function rnd(n, p)   local m=10^(p or 0); return math.floor(n*m+0.5)/m  end
function sort(t,f)   table.sort(t,f); return t end
function splice( t, i, j, k,    u) 
  u={}; for n=(i or 1)//1, (j or #t)//1, (k or 1)//1 do u[1+#u]=t[n] end return u end
--------------------------------------------------------------------------------
-- ## Demos
local no,go,fails = {},{},0

function go.the() oo(the) end

function go.ranges(       rows,n,m,bests,rests)
  math.randomseed(the.Seed)
  lrows = ROWS(the.file)
  sort(rows.all)
  n=#rows.all
  m=n^the.min  
  bests = splice(rows.all, 1,  m)
  rests = splice(rows.all, n - m)
  rows:bins(bests,rests) end

--------------------------------------------------------------------------------
-- ## RETURN MODULE
if   pcall(debug.getlocal, 4, 1) then return {
            o=o,oo=oo,the=the,EGS=EGS,NUM=NUM,RANGE=RANGE,
            ROW=ROW,ROWS=ROWS,SOME=SOME,SYM=SYM} end
--------------------------------------------------------------------------------
-- ## MAIN START UP
for k,v in pairs(the) do the[k]=cli(k,v) end
if the.help then os.exit(print(
  help:gsub("[%u][%u%d]+", "\27[31m%1\27[0m")           -- highlight capitals
      :gsub("\"[^\"]+\"", "\27[32m%1\27[0m")            -- highlight strings  
      :gsub("(%s)([-][-]?[^%s]+)(%s)","%1\27[33m%2\27[0m%3")))--highlight flags
end

-- Change anything from here done (except last 2 lines)
local defaults,todo=going(the,go)
for _,one in pairs(todo) do goes(go[one],defaults,the) end

-- Last two lines. Do not change. Check for rogues and report any failures.
for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end--[5]
os.exit(fails)
