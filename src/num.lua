local ako,_   =require"ako", require"lib"
local obj,new = _.obj, _.new 

local NUM = obj"NUM"
function NUM:new(at,name)   
  name=name or ""
  return new(NUM, {at=at or 0, name=name, 
            indep=not ako.goal(name), 
            n=0, has={}, nump=true, n=0, w = ako.weight(name or ""), 
            lo=math.huge, hi=-math.huge, mu=0, m2=0, sd=0, bins={}}) end

function NUM:add(x,   d)
  if x ~= "?" then
    self.n  = self.n+1
    self.lo = math.min(x, self.lo)
    self.hi = math.max(x, self.hi) 
    d       = x - self.mu
    self.mu = self.mu + d/self.n
    self.m2 = self.m2 + d*(x - self.mu)
    self.sd = ((self.m2<0 or self.n<2) and 0) or ((self.m2/(self.n -1))^0.5) end
 return x end

return NUM
