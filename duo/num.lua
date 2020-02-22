-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- ---------

local Num=require("thing"):extend()

function Num:inits(t)
  Num.super.inits(self,t)
  self.mu, self.m2, self.sd= 0,0,0
  self.lo  = math.maxinteger
  self.hi  = math.mininteger
  return self
end

function Num:mid() return self.mu end
function Num:var() return self.sd end

function Num:add(x) 
  local d       = x - self.mu
  self.mu = self.mu + d/self.n
  self.m2 = self.m2 + d*(x - self.mu)
  self.sd = self:sd0()
  if x > self.hi then self.hi = x end
  if x < self.lo then self.lo = x end
end

function Num:sub(x)
  local d = x - self.mu
  self.mu = self.mu - d/i.n
  self.m2 = self.m2 - d*(x- self.mu)
  self.sd = self:sd0()
end

function Num:sd0()
  if     self.n  < 2 then return 0 
  elseif self.m2 < 0 then return 0
  else   return (self.m2 / (self.n - 1))^0.5 end
end

return Num
