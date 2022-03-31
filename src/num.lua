local the,ako,_   = require"the", require"ako", require"lib"
local obj,new = _.obj, _.new 

local NUM = obj"NUM"
function NUM.new(at,name)   
  name=name or ""
  return new(NUM, {at=at or 0, name=name, 
            indep=not ako.goal(name), 
            n=0, _has={}, nump=true, n=0, w = ako.weight(name or ""), 
            lo=math.huge, hi=-math.huge, mu=0, m2=0, sd=0, bins={}}) end

local r=math.random
function NUM:add(x,   d)
  if x ~= "?" then
    self.n  = self.n+1
    self.lo = math.min(x, self.lo)
    self.hi = math.max(x, self.hi) 
    d       = x - self.mu
    self.mu = self.mu + d/self.n
    self.m2 = self.m2 + d*(x - self.mu)
    self.sd = ((self.m2<0 or self.n<2) and 0) or ((self.m2/(self.n -1))^0.5) 
    if #i._has < the.some then push(i._has,x) 
    elseif r() < the.some/self.n then i._has[1 + (r()*#i.has) // 1] = x end end
 return x end

function NUM:div() return i.sd end
function NUM:mid() return i,mu end

function NUM:same(x,y) return math.abs(x - y) <= the.cohen * self.sd end

return NUM
