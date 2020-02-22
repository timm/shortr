local Class=require "30log"
local Object = Class("Object") 

function Object:init(t)  self:inits(t or {}); return self; end
function Object:inits(t) return self end
function Object.__tostring(self)
  local s, sep, lst, t = "", "", {}, self or {}
  for k,v in pairs(t) do
    if  k ~= "class" and k ~= "super" then
      if "_" ~= string.sub(k, 1, 1) then
        lst[#lst+1] = k end end end
  table.sort(lst)
  for k=1,#lst do
    s = s .. sep .. lst[k] .. "=" .. tostring(t[lst[k]]) 
    sep=", " 
  end 
  return (self.name or 'Object') .. "{" .. s .. "}"
end

local Sym=Object:extend()
function Sym:inits(t)
  self.n      = 0
  self.pos    = t.pos or 0
  self.txt    = t.txt or ""
  self.counts = {}
  self.most   = 0
  self.mode   = nil
  self.ent    = nil
end

function Sym:__add(x)
  if x=="?" then return x end
  self.n = self.n + 1
  self.ent= nil
  local d = (self.counts[x] or 0) + 1
  self.counts[x] = d
  if d > self.most then self.most, self.mode = d, x end
  return self
end

local Num=Object:extend()
function Num:inits(t)
  self.n   = 0
  self.pos = t.pos or 0
  self.txt = t.txt or ""
  self.mu, self.m2, self.sd= 0,0,0
  self.lo  = math.maxinteger
  self.hi  = math.mininteger
  return self
end

function Num:__add(x) 
  if x == "?" then return x end
  self.n  = self.n + 1
  local d       = x - self.mu
  self.mu = self.mu + d/self.n
  self.m2 = self.m2 + d*(x - self.mu)
  self.sd = self:sd0()
  if x > self.hi then self.hi = x end
  if x < self.lo then self.lo = x end
  return self
end

function Num:sd0()
  if     self.n  < 2 then return 0 
  elseif self.m2 < 0 then return 0
  else   return (self.m2 / (self.n - 1))^0.5 end
end

function adds(lst, what, x)
  for _,v in pairs(lst) do
    what = what or (type(v)=="number" and Num or Sym)
    x    = x or what()
    x    = x + v
  end
  return x   
end

print(adds {1,2,3,4,5})
print (adds {"a", "a", "a", "b","b","c"})


