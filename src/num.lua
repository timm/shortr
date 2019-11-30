-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

function aa()
local Column = require("column")
local Num   = {is="Num"}

function Num.new(t)
  local i = Column.new(t)
  i.me = Num
  i.mu  = 0
  i.m2  = 0
  i.sd  = 0
  t     = t or {}
  i.key = t.key or function (z) return z end
  i.lo  = math.maxinteger
  i.hi  = math.mininteger
  return i
end

function Num.sd(i)
  if     i.n  < 2 then return 0 
  elseif i.m2 < 0 then return 0
  else  return (i.m2/(i.n - 1))^0.5 end
end

function Num.add(i,x,    d) 
  if x == "?" then return x end
  x = i.key(x)
  i.n  = i.n + 1
  d    = x - i.mu
  i.mu = i.mu + d/i.n
  i.m2 = i.m2 + d*(x - i.mu)
  if x > i.hi then i.hi = x end
  if x < i.lo then i.lo = x end
  i.sd = Num.sd(i)
end

function Num.sub(i,x,   d) 
  if (x == "?") then return x end
  x = i.key(x)
  if (i.n == 1) then return x end
  i.n  = i.n - 1
  d    = x - i.mu
  i.mu = i.mu - d/i.n
  i.m2 = i.m2 - d*(x- i.mu)
  i.sd = Num.sd(i)
  return x
end

function Num.norm(i,x) 
  return x=="?" and 0.5 or (x-i.lo) / (i.hi-i.lo + 10^-32)
end

function Num.xpect(i,j,  n)  
  n = i.n + j.n +0.0001
  return i.n/n * i.sd+ j.n/n * j.sd
end

return Num
