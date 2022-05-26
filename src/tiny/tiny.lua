local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local atom,big,cli,csv,is,map,o,oo,push,rand,rnd,sort,splice,the,tothing
local EGS, NUM, ROW, ROWS, SOME, SYM
local the={p    = 2,
           bins = 16,
           min  = .5,
           file = "../../etc/data/auto93.csv",
           some = 512}
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
    d[key] = atom(x) end 
  return d end 

the = cli(the)
--------------------------------------------------------------------------------
big = math.huge
rand= math.random
function rnd(n, p) local m=10^(p or 0); return math.floor(n*m+0.5)/m  end

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
function NUM.div(i,  a) a=i.all:has(); return (per(a,.9) - per(a,.1))/2.56 end
function NUM.norm(i,x)
  return x=="?" and x or i.hi-i.lo<1E-9 and 0 or (x - i.lo)/(i.hi - i.lo) end

local _bins
function NUM.bin(i,v,  b) b=(i.hi-i.lo)/the.bins;return math.floor(v/b+0.5)*b end
function NUM.bins(i,bins) _bins(sort(bins,lt"lo"),1,#bins-1,i.n^the.min, 1) end

-- gotta make a new range with x and y. need a num merge
function _bins(bins, lo,hi, enough, rank)
  lhs, rhs, all = SYM(), SYM(), SYM()
  for _,bin in pairs(bins) do 
    for x,n in pairs(bin.y.all) do 
       all:add(x,n); rhs:add(x,n) end end
  local n,best,cut = rhs.n, rhs:div()
  for j=lo,hi do
    for x,n in pairs(bins[j].y.all) do lhs:add(x,n); rhs:sub(x,n) end 
    if rhs.n >= enough and lhs.n >= enough then
      local tmp= rhs:div()*rhs.n/n + lhs:div()*lhs.n/n 
      if tmp < best*1.01 then cut,best =j,tmp end end end
  if cut 
  then rank = _bins(bins, lo, cut,   enough, rank) + 1
       rank = _bins(bins, cut+1, hi, enough, rank)
  else for _,bin in pairs(bins) do bin.rank = rank end end
  return rank end 
   
 
    


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
    i.n = i.n-inc
    i.all[x] = i.all[x]  - inc end end
 
function SYM.mid(i) return i.mode end
function SYM.div(i,   e)
  e=0; 
  for k,n in pairs(i.all) do if n>0 then e=e-n/i.n*math.log(n/i.n,2) end end 
  return e end

function SYM.clone(i) return SYM(i.at,i.txt) end
function SYM.bin(i,x) return x end
function SYM.bins(i,ranges) return ranges end
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
function bins(col,...)
  local tmp,out={},{}
  for klass,rows in pairs{...} do
    for _,row in pairs(rows) do
      local v = row.cells[col.at]
      if v~="?" then 
        local bin = col:bin(v)
        tmp[bin]  = tmp[bin] or push(out, {x=col:clone(),y=SYM(col.at, col.txt)})
        tmp[bin].x:add(v)
        tmp[bin].y:add(v) end end end
   col:bins(out) 
   return out end
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
--------------------------------------------------------------------------------
local rows = ROWS(the.file)
sort(rows.all)
oo(rows:clone(splice(rows.all,1,40)):mid(0))
oo(rows:clone(splice(rows.all,#rows.all - 40)):mid(0))

for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end--[5]
