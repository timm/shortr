-- basic NB. discretized data. class is last thing on each line
local _,the = require"tricks", require"the"
local class, new = _.class, _.new
local Egs,Cols,Nominal,Ratio= class"Egs",class"Cols",class"Nominal",class"Ratio"

function Nominal:new(at,txt)
  return new({at=at or 0, name=name or "", n=0, has={}},Nominal) end

function Nominal.add(i,x)
  if x ~= "?" then i.n =i.n+1; i.has[x] = 1+(i.has[x] or 0) end end

function Ratio:new(at,txt)
  return new({at=at or 0, name=name or "", n=0, mu=0,m2=0,sd=0},Ratio) end

function Ratio.add(i,x)
  if x ~= "?" then 
    i.n =i.n+1
    local d= x - i.mu
    i.mu = i.mu + d/i.n
    i.m2 = i.m2 + d*(x - i.mu)
    i.sd = (i.m2<0 or i.n<2) and 0 or ((i.m2/(i.n - 1))^0.5)
    i.lo = i.lo and math.min(x, i.lo) or x
    i.hi = i.hi and math.max(x, i.hi) or x end end

function Cols:new(names)
  local i   = new({names=names, klass=nil,all={}, x={}, y={}}, Cols)
  local is  = {}
  is.ratio  = function(x) return x:find"^[A-Z]" end
  is.goal   = function(x) return x:find"[-+!]" end
  is.klass  = function(x) return x:find"!$" end
  is.ignore = function(x) return x:find":$" end
  for at,name in pairs(names) do
    col         = (is.ratio(name) and Ratio or Nominal)(at,name) 
    col.is_goal = is.goal(name)
    push(i.all, col)
    if not is.ignore(name) then
      if is.klass(name) then i.klass = col end
      push(is.goal(name) and i.y or i.x, col) end end
  return i end

function Egs:new()
  return new({rows={}, cols=nil}, Egs) end

function Egs:add(i,t)
  t = t.cells or t -- detail (for future extension)
  if   not i.cols 
  then i.cols = Cols(t) -- handle row1
  else push(i.rows, t)  -- handle other rows
       for _,col in pairs(i.all) do col:add(t[col.at]) end end end
  
return {Egs=Egs, Ratio=Ratio, Nominal=Nominal}
