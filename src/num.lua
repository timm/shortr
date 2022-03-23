local num = {}
local ako = require"ako"

function num.new(at,name)   
  return {nump=true, indep=false, n=0, at=at or 0, name=name or "", 
          w = ako.weight(name or ""), lo=math.huge, hi=-math.huge, 
          mu=0,m2=0,sd=0,bins={}} end

function num.add(i,x,   d)
  if x ~= "?" then
    i.n = i.n+1
    i.lo = math.min(x, i.lo)
    i.hi = math.max(x, i.hi) 
    d      = x - i.mu
    i.mu = i.mu + d/i.n
    i.m2 = i.m2 + d*(x - i.mu)
    i.sd = ((i.m2<0 or i.n<2) and 0) or ((i.m2/(i.n - 1))^0.5) end
 return x end

return num
