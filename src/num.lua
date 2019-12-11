-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

-- Incrementally track a stream of numbers, mainitaning
-- the lowest (`i.lo`), highest (`i.hi`) 
-- seen so far (as well as the mean `i.mu`
-- and standard deviation `i.sd`).

local Column = require("column")
local THE   = require("the")
local Num   = {is="Num"}

function Num.var(i) return i.sd end
function Num.mid(i) return i.mu end

function Num.xpect(i,j,  n)  
  n = i.n + j.n +0.0001
  return i.n/n * i.sd+ j.n/n * j.sd 
end

function Num.all(lst,key)
  local i = Num.new{key=key}
  for _,x in pairs(lst) do Num.add(i, x) end
  return i
end


-- To customize what kinds of numbers we track, change `i.key`
-- to some selector function.

function Num.new(t)
  local i = Column.new(t)
  i.me = Num
  i.mu  = 0
  i.m2  = 0
  i.sd  = 0
  t     = t or {}
  i.key = t.key or function (z) return z end
  i.lo  = t.lo or math.maxinteger
  i.hi  = t.hi or math.mininteger
  return i
end

-- Bulk addition to a `Num`.
-- Add one item to a `Num`.
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

-- Also supported, removing numbers from a stream.
-- Note: this code does not update `i.lo` and `i.hi`.
-- Also this method has a well-known numerical methods
-- problem when `i.n` gets small and the `i.mu` approaches zero.
-- So avoid using this for `i.n` under 8.

function Num.sub(i,x,   d) 
  if (x == "?") then return x end
  x = i.key(x)
  if (i.n == 1) then return x end
  i.n  = i.n - 1
  d    = x - i.mu
  i.mu = i.mu - d/i.n
  i.m2 = i.m2 - d*(x- i.mu)
  i.sd = Num.sd(i)
end

-- Compute standard deviation. Dodge hard cases.

function Num.sd(i)
  if     i.n  < 2 then return 0 
  elseif i.m2 < 0 then return 0
  else  return (i.m2/(i.n - 1))^0.5 end
end


-- Return a number `x` normalized to the range 0..1, 
-- `i.lo..i.hi`.

function Num.norm(i,x) 
  return x=="?" and 0.5 or (x-i.lo) / (i.hi-i.lo + 10^-32)
end

-- -----------
function Num.dist(i,x,y,   nox,noy,z)
  nox = x == THE.char.skip
  noy = y == THE.char.skip
  if nox and noy then return 1 
  elseif nox then y= Num.norm(i,y); x= y>0.5 and 1 or 0 
  elseif noy then x= Num.norm(i,x); y= x>0.5 and 1 or 0 
  else            x= Num.norm(i,x)
                  y= Num.norm(i,y)
  end
  return x>y and x-y or y-x
end

-- ----------
-- And finally...


return Num
