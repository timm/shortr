local the={ 
  file="auto93.csv",
  p=2,
  far=.9,
  see=10019,
  some=512}

--------------------------------------------------------------
local push,fmt
fmt=string.format
function push(t,x) table.insert(t,x); return x end

local thing,things,file2things
function thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return tonumber(x) or x end

function things(x,sep,  t)
  t={}; for y in x:gmatch(sep or"([^,]+)") do push(t,thing(y)) end
  return t end

function file2things(file,      x,n)
  n, file = -1, io.input(file)
  return function()
    x=io.read(); 
    n=n+1; if x then return n,things(x) else io.close(file) end end end

local firsts,sort, map,slots
function firsts(a,b) return a[1] < b[1] end
function last(a) return a[#a] end
function per(a,p) return a[ (p*#a)//1 ] end
function sort(t,f)    table.sort(t,f); return t end
function map(t,f, u)  u={};for k,v in pairs(t) do push(u,f(v)) end; return u end
function slots(t, u,s)
  u={}
  for k,v in pairs(t) do s=tostring(k);if s:sub(1,1)~="_" then push(u,k) end end
  return sort(u) end

local oo,o
function oo(t) print(o(t)) end
function o(t)
  if type(t)~="table" then return tostring(t) end
  local key=function(k) return fmt(":%s %s",k,o(t[k])) end
  --local u = map(slots(t),key)
  local u = #t>0 and map(t,o) or map(slots(t),key)
  return '{'..table.concat(u," ").."}" end

local function egs(file)
  local i = {rows={},cols={w={},hi={},lo={},use={}, names={}}}
  for n,row in file2things(file) do
    if n==0 then
      i.cols.names=row
      for c,txt in pairs(row) do
        if not txt:find":$" then
          i.cols.use[c] = c
          if txt:find"^[A-Z]" then i.cols.hi[c], i.cols.lo[c] = -1E31, 1E31 end 
          if txt:find"-$"     then i.cols.w[c] = -1 end
          if txt:find"+$"     then i.cols.w[c] =  1 end end end 
    else
      push(i.rows,row)
      for c,_ in pairs(i.cols.hi) do
        if row[c] ~= "?" then
          i.cols.hi[c] = math.max(row[c], i.cols.hi[c])
          i.cols.lo[c] = math.min(row[c], i.cols.lo[c]) end end end end 
  return i end

local norm,dist,neighbors, far,furthest
function norm(i,c,x) 
  lo,hi=i.cols.lo[c],i.cols.hi[c]; return hi-lo<1E-9 and 0 or (x-lo)/(hi-lo) end

function dist(i, row1,row2,  n,inc,d,n)
  local function nump(i,c) return i.cols.hi[c] end
  d,n = 0,0    
  for _,c in i.cols.use do
    a,b = row1[c],row2[c]
    if   a=="?" and b=="?" 
    then inc=1
    else if   nump(i,c) 
         then if     a=="?" then b=norm(i,c,a); a=b<.5 and 1 or 0 
              elseif b=="?" then a=norm(i,c,b); b=a<.5 and 1 or 0
              else   a,b = norm(i,c,a), norm(i,c,b) end
              inc = math.abs(a - b) 
         else inc = a==b and 0 or 1 end end
    d = d+inc*the.p
    n = n+1 end 
  return (d/n)^(1/p) end

function neighbors(i, r1,rows)
  return sort(map(rows,function(r2) return {dist(i,r2,r2),r2} end),firsts) end

function far(i,r1,rows)      return per(neighbors(i,r1,rows),the.far)[2] end
function furthest(i,r1,rows) return last(neighbors(i,r1,rows))[2] end

  
it = egs(the.file)
oo(egs(the.file).cols)

