for k,__ in pairs(_ENV) do b4[k]=k end 
local the={}
local big,csv,fmt,map,o,oo,push,rand,rnd,rnds,slice,string2thing
local COL,ROW,COLS,EGS
local col,row,cols,egs
--------------------------------------------------------------------------------
big=math.huge
rand=math.random
fmt=string.format
function map(t,f, u)  u={};for k,v in pairs(t) do u[1+#u]=f(v) end; return u end
function push(t,x)    t[1+#t]=x; return x end
function slice(t,i,j,k,     u) 
  i,j = (i or 1)//1, (j or #t)//1
  k   = (k and (j-i)/k or 1)//1
  u={}; for n=i,j,k do u[1+#u] = t[n] end return u end

-- "Strings 2 things" coercion. 
function string2thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x  end

function csv(csvfile) 
  csvfile = io.input(csvfile)
  return function(line, row) 
    line=io.read()
    if not line then io.close(csvfile) else
      row={}; for x in line:gmatch("([^,]+)") do push(row,string2thing(x)) end
      return row end end end 

-- "Things 2 strings" coercion.
function oo(t) print(o(t)) end
function o(t,    u)
  if #t>0 then return "{"..table.concat(map(t,tostring)," ").."}" else
    u={}; for k,v in pairs(t) do u[1+#u] = fmt(":%s %s",k,v) end
    return (t.is or "").."{"..table.concat(sort(u)," ").."}" end end

function rnds(t,f) return map(t, function(x) return rnd(x,f) end) end
function rnd(x,f) 
  return fmt(type(x)=="number" and (x~=x//1 and f or the.rnd) or"%s",x) end
--------------------------------------------------------------------------------
function row.new(cells,of) return {cells=cells, of=of, cooked={}} end
function egs.new()         return {rows={},   cols=nil} end

function new(klass,t) klass.__tostring=o; return setmetatable(t,klass) end

function Col.__call(at,txt,    i)
  i = new(Col,{at=at or 0, txt=txt or "", n=0, w=1, goalp=false})
  if i.txt:find"-$"     then i.w=-1       end
  if i.txt:find"[!-+]$" then i.goalp=true end 
  if i.txt:find"^[A-Z]" then i.lo,i.hi=big,-big else i.syms={} end 
  return i  end 

function Col.add(i,x) 
  if x=="?" then  return x end
  col.n = col.n + 1
  if   col.syms 
  then col.syms[x]=1+(cols.syms[x] or 0)
  else col.lo = math.min(x, col.lo)
       col.hi = math.max(x, col.hi) end 
  return i end

function Col.norm(i,x) 
  if x=="?" or i.syms then return x end
  return i.hi-i.lo<1E-9 and 0 or(x-i.lo)/(i.hi-i.lo)end

function Cols.__call(names)
  i = new(Cols,{name=names,    syms={}, nums={}, all={},
                           x= {syms={}, nums={}, all={}},
                           y= {syms={}, nums={}, all={}}})
  for at,txt in pairs(i.names) do 
    col = Col(at,txt)
    for _,here in pairs{i, col.goalp and i.y or i.x} do
      push(here.all, col)
      if not txt:find":$" then 
        push(col.syms and here.syms or here.nums,col) end end end 
  return i end

local Egs,Cols={},{}

function Egs.__call(src,        i)
  i= new(Egs,{rows={},cols=nil})
  if   src==nil or type(src)=="string" 
  then for   row in csv(src)  do i:add(row) end
  else for _,row in pais(src) do i:add(row) end end
  return i end

function Egs.add(i,row)
  if   not i.cols 
  then i.cols = cols(COLS(row))
  else row = push(i.rows, row.cells and row or Row(row,i))
       for _,col in pairs(i.cols.all) do col:add(row.cells[col.at]) end
  return i end


