-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Num = require("thing"):extend("Num")

function Num:has() return {
  pos=0, txt="",
  w=1,
  mu=0, m2=0, n=0, sd=0,
  lo= math.maxinteger,
  hi= math.mininteger}
end

function Num:mid() return self.mu end
function Num:var() return self.sd end

function Num:add1(x) 
  local d       = x - self.mu
  self.mu = self.mu + d/self.n
  self.m2 = self.m2 + d*(x - self.mu)
  self.sd = self:sd0()
  if x > self.hi then self.hi = x end
  if x < self.lo then self.lo = x end
end

function Num:sub1(x)
  local d = x - self.mu
  self.mu = self.mu - d/self.n
  self.m2 = self.m2 - d*(x- self.mu)
  self.sd = self:sd0()
end

function Num:sd0()
  if     self.n  < 2 then return 0 
  elseif self.m2 < 0 then return 0
  else   return (self.m2 / (self.n - 1))^0.5 end
end

return Num
