_=require"tricks"
local class,lines,map,push = _.class,_.lines,_.map,_.push

-- ## Classes
local SYM, NUM, EGS = class"SYM", class"NUM", class"EGS"

-- ### Sym
-- Summarize symbolic columns
function SYM:new(at,name) 
    return new({at=at, name=name, most=0,n=0,all={}}, SYM) end

-- ### Num
-- Summarize numeric columns
function NUM:new(at,name) 
    return new({at=at, name=name, _all={}, 
                w=(name or ""):find"-$" and -1 or 1,
                n=0, sd=0, mu=0, m2=0, lo=math.huge, hi=-math.huge}, NUM) end

-- ### EGS
-- Store rows, summarized into columns (and columns are divided into
-- dependent columsns `y` and indepednent columns `x`,
function EGS:new(names,  i,col)
  i = new({_all={}, cols={names=names, all={}, x={}, y={}}}, EGS)
  for at,name in pairs(names) do
    col = push(i.cols.all, (name:find"^[A-Z]" and NUM or SYM)(at,name) )
    if not name:find":$" then
      if name:find"!$" then i.cols.class = col end 
      push(name:find"[-+!]$" and i.cols.y or i.cols.x, col) end end
  return i end

function EGS:new4file(file,  i)
  for row in things(the.file) do 
    if i then i:add(row) else i = EGS(row) end end 
  return i end

--  ## Copy
function EGS.copy(i,rows,    j) 
  j = EGS(i.cols.names)
  for _,row in pairs(rows or {}) do j:add(row) end 
  return j end

-- ## Update
function EGS.add(i,row)
  push(i._all,  row)
  for at,col in pairs(i.cols.all) do col:add(row[col.at]) end end 

function SYM.add(i,x,inc)
  if x ~= "?" then
    inc = inc or 1
    i.n = i.n+inc
    i.all[x] = inc + (i.all[x] or 0)
    if i.all[x] > i.most then i.most, i.mode = i.all[x], x end end end

function NUM.add(i,x,_,    d,a)
  if x ~="?" then
    i.n   = i.n + 1
    d     = x - i.mu
    i.mu  = i.mu + d/i.n
    i.m2  = i.m2 + d*(x - i.mu)
    i.sd  = (i.m2<0 or i.n<2) and 0 or ((i.m2/(i.n - 1))^0.5)
    i.lo  = math.min(x, i.lo)
    i.hi  = math.max(x, i.hi) 
    a     = i._all
    if     #a  < the.keep     then i.ok=false; push(a,x)  
    elseif r() < the.keep/i.n then i.ok=false; a[r(#a)]=x end end end

-- ## Query
 
-- Central tendency. 
function EGS.mid(i,cols)
  return map(cols or i.cols.y, function(col) return col:mid() end) end

function NUM.mid(i) return i.mu end

function SYM.mid(i) return i.mode end

-- Diversity around the central tendancy
function EGS.div(i,cols)
  return map(cols or i.cols.y, function(col) return col:div() end) end

function NUM.div(i) return i.sd end

function SYM.div(i,  e)
  e=0; for _,n in pairs(i.all) do
         if n > 0 then e = e - n/i.n * math.log(n/i.n,2) end end
  return math.abs(e) end

-- Maths queries
function NUM.norm(i,x)
  return i.hi - i.lo < 1E-32 and 0 or (x - i.lo)/(i.hi - i.lo) end 

function NUM.all(i)
  if not i.ok then table.sort(i._all); i.ok=true end
  return i._all end

return {NUM=NUM, SYM=SYM, EGS=EGS}
