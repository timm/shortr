local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local m={}

---- ---- ---- ---- Config
local the = {
       p=2,
       bins=7,
       seed=10019,
       min=.5,
       far=.95,
       files="../../data/auto93.csv"}

m.config={the}

---- ---- ---- ---- Types
local    is,col0,row0,cols0,rows0
m.types={is,col0,row0,cols0,rows0}

function is(str,key) 
  local patterns={num  = "^[A-Z]", 
                  goal = "[!+-]$", 
                  klass= "!$", 
                  skip = ":$", 
                  less = "-$"}
  return (str or ""):find(patterns[key]) end

function col0(num,txt)
  return {n    = 0,
          at   = num or 0, 
          txt  = txt or "",
          nump = is(txt,"num"), 
          w    = is(txt,"less") and -1 or 1,
          ok   = false,
          has = {}} end

function row0(cols, t)
  return {cols=cols,cells=t,cooked=t} end

function cols0(names)
  local i = {names=names,all={}, x={}, y={}, klass=nil}
  for at,txt in pairs(names) do 
    local one = push(i.all, col0(at,txt))
    if not is(txt,"skip") then
      push(is(txt,"goal") and i.y or i.x, one)
      if is(txt,"klass") then i.klass=one end end end 
  return i end

function rows0() return {has={},cols=nil} end

---- ---- ---- ---- Methods
---- ---- ---- Update
local     add,adds,row,clone
m.update={add,adds,row,clone}

function add(col,x)
  if x ~= "?" then
   col.n = col.n + 1
   col.ok = true
   if col.nump then push(col.has,x); col.ok=false 
               else col.has[x] = 1 + (col.has[x] or 0) end end end

function adds(cols,t)
  t = t.cells and t or row0(cols,t)
  for _,cols in pairs{cols.x,cols.y} do
    for _,col in pairs(cols) do add(col, t.cells[col.at]) end end 
  return t end

function row(rows,t)
 if   rows.cols 
 then push(rows.has, adds(rows.cols,t))
 else rows.cols = cols0(t) end end

function clone(rows,t)
  local new= rows0()
  row(new, rows.cols.names)
  for _,row1 in pairs(t or {}) do row(new,row1) end
  return new end

---- ---- ---- Query
local    has,norm,mid,div,stats,better
m.query={has,norm,mid,div,stats,better}

function has(col)
  if not col.ok then table.sort(col.has) end
  col.ok = true
  return col.has end

function norm(col,x)
  local a= has(col)
  return a[#a] - a[1] < 1E-9 and 0 or (x-a[1])/(a[#a]-a[1]) end

function mid(col)
  local mode,most= -1,nil
  if col.nump then return per(has(col),.5)  end
  for x,n in pairs(col.has) do if n>most then mode,most=x,n end end
  return mode end

function div(col)
  local ent=0
  if col.nump then return (per(has(col),.9)-per(has(col),.1))/2.58 end
  for _,n in pairs(col.has) do 
    if n>0 then ent=ent-n/col.n*math.log(n/col.n,2) end end
  return ent end

function stats(rows,places,f,cols,   u)
  f =  f or mid
  cols = cols or rows.cols.y
  u={}
  for k,col in pairs(rows.cols.y) do 
    u.n=col.n; u[col.txt]=rnd(f(col),places) end; 
  return u end

function better(row1,row2)
  local s1,s2,d,n=0,0,0,0
  local ys,e = row1.cols.y,math.exp(1)
  for _,col in pairs(ys) do
    x,y= row1.cells[col.at], row2.cells[col.at]
    x,y= norm(col,x), norm(col,y)
    s1 = s1 - e^(col.w * (x-y)/#ys)
    s2 = s2 - e^(col.w * (y-x)/#ys) end
  return s1/#ys < s2/#ys end 

---- ---- ---- Dist
local   dist,around,far, halve,semisort
m.dist={dist,around,far,halve,semisort}

function dist(row1,row2)
  local d,n,x,y,dist1=0,0
  function dist1(col,x,y)    
    if x=="?" and y=="?" then return 1 end
    if   col.nump 
    then if     x=="?" then y=norm(col,y); x=y<.5 and 1 or 0 
         elseif y=="?" then x=norm(col,x); y=x<.5 and 1 or 0 
         else   x,y = norm(col,x), norm(col,y) end
         return math.abs(x-y) 
    else return (x=="?" or y=="?") and 1 or x==y and 0 or 1 end 
  end ---------------
  for _,col in pairs(row1.cols.x) do
    x,y = row1.cells[col.at], row2.cells[col.at]
    d = d+dist1(col,x,y)^the.p
    n = n + 1 end
  return (d/n)^(1/the.p) end

function around(r1,t)
  return sort(map(t,function(r2) return {r=r2,d=dist(r1,r2)} end),lt"d") end

function halve(t,old)
  local function poles(t,old)
    local A,B
    local function far(r1,t) return per(around(r1,t), the.far).r end
    A= old or far(any(t),t)
    B= far(A,t)
    return A,B,dist(A,B) 
  end ---------------------
  local A,B,c = poles(t,old)
  local project=function(r) return {r=r,d=(dist(r,A)^2 - c^2 + dist(r,B)^2)/(2*c)} end
  local As,Bs = {},{}
  for n,rd in pairs(sort(map(t, project),lt"d")) do 
    push(n < #t/2 and Bs or As, rd.r) end
  return A,B,As,Bs,c end

function semisort(t,old,stop,out)
  stop = stop or (#t)^the.min
  out  = out or {}
  if #t < stop then
    for _,row in pairs(t) do push(out,row) end
    return out end
  local A,B,As,Bs = halve(t,old)
  if better(A,B) then 
    for _,row in pairs(reverse(Bs)) do push(out,row) end
    return semisort(reverse(As),A,stop,out)
  else
    for _,row in pairs(As) do push(out,row) end
    return semisort(Bs,B,stop,out) end end 

---- ---- ---- ---- Library
m.lib.lists={}
m.lib.string={}
m.lib.read={}
m.lib.write={}

local  rand
m.lib.maths={rand}
rand=math.random

local        fmt
m.lib.print={fmt}
fmt = string.format

function rev(t)
  for i=1, math.floor(#t / 2) do t[i],t[#t-i+1] = t[#t-i+1],t[i] end
  return t end

function sort(t,f) table.sort(t,f); return t end
function lt(x) return function(a,b) return a[x] < b[x] end end
function gt(x) return function(a,b) return a[x] > b[x] end end

function push(t,x) t[1+#t]=x; return x end 
function map(t,f) 
  local u={}; for _,v in pairs(t) do u[1+#u]=f(v) end; return u end

function any(a) return a[rand(#a)] end
function rnd(x, places)  
  local mult = 10^(places or 2)
  return math.floor(x * mult + 0.5) / mult end

function per(t,p) 
  p=math.floor((p*#t)+.5); return t[math.max(1,math.min(#t,p))] end

function coerce(x)
  x = x:match"^%s*(.-)%s*$" 
  if x=="true" then return true elseif x=="false" then return false 
  else return math.tointeger(x) or tonumber(x) or x end  end

function chat(t) print(cat(t)) return t end 
function cat(t,   show,u)  
  if type(t)~="table" then return tostring(t) end
  function show(k,v) return #t==0 and fmt(":%s %s",k,v) or tostring(v) end
  u={}; for k,v in pairs(t) do u[1+#u]=show(k,v) end
  return (t._is or "").."{"..table.concat(#t==0 and sort(u) or u," ").."}" end

function words(s,sep,fun,      t)
  fun = fun or function(z) return z end
  sep = fmt("([^%s]+)",sep)
  t={};for x in s:gmatch(sep) do t[1+#t]=fun(x) end;return t end 

function lines(file, fun)
  local src = io.input(file)
  while true do
    local line = io.read()
    if not line then return io.close(src) else fun(line) end end end 

function csv(file, fun)
  lines(file, 
    function(line) fun(words(line,",",coerce)) end) end

function csv2rows(file,rows)
  rows=rows0()
  csv(file, function(t) row(rows,t) end) 
  return rows end

---- ---- ---- ---- Start
local go={}
function go.one(rows1,rows2)
  rows1=csv2rows("../../data/auto93.csv")
  print("mid1", cat(stats(rows1,2,mid))) 
  print("div1", cat(stats(rows1,2,div)))
  rows2=clone(rows1,rows1.has) 
  print("mid2", cat(stats(rows2,2,mid))) 
  print("div2", cat(stats(rows2,2,div)))
  end

function go.dist(rows,row1,row2)
  rows= csv2rows("../../data/auto93.csv")
  for i = 1,20 do
    row1=any(rows.has)
    row2=any(rows.has)
    print(dist(row1,row2)) end end


math.randomseed(the.seed)
go.one()
--go.dist()
for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end
