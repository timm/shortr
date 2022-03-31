local _,the,COL = require"lib", require"the", require"col"
local class = _.class
local sort,upx = _.sort, _.upx

local NUM = class("NUM",COL)
function NUM:new(at,name)   
  self:super(at,name)
  self.has, self.ok = {}, false
  self.lo,self.hi =  math.huge, -math.huge end

local r=math.random
function NUM:add1(x,   d)
  self.lo = math.min(x, self.lo)
  self.hi = math.max(x, self.hi) 
  if     #i.has < the.some     then self.ok=false; push(i.has,x) 
  elseif r() < the.some/self.n then self.ok=false; i.has[1+((r()*#i.has)//1)]=x end end

function NUM:div(  a)  a=self:all(); return (per(a,.9) - per(a,.1))/2.56 end
function NUM:mid()     return i,mu end
function NUM:same(x,y) return math.abs(x - y) <= the.cohen * self.sd end

function NUM:dist1(x,y)
  if     x=="?" then y = norm(self.lo, self.hi, y); x=y<.5 and 1 or 0 
  elseif y=="?" then x = norm(self.lo, self.hi, x); y=x<.5 and 1 or 0
  else             x,y = norm(self.lo, self.hi, x), norm(self.lo, self.hi,y) end
  return math.abs(x-y) end
  
function NUM:like1(i,x,_)
  local sd= self:div()
  if x < self.mu - 4*sd then return 0 end
  if x > self.mu + 4*sd then return 0 end
  local denom = (math.pi*2*sd^2)^.5
  local nom   =  math.exp(1)^(-(x-self.mu)^2/(2*sd^2+1E-32))
  return nom/(denom + 1E-32) end

function NUM:merge(other,   out)
  out = NUM(self.at, self.name)
  for _,x in self(self.has) do out:add(x) end
  for _,x in self(other.has) do out:add(x) end
  return out end

function NUM:all()
  if not self.ok then table.sort(i.has) end
  self.ok=true
  return i.has end

local div,merge
function NUM:delta(other)
  local xys = {}
  for _,x in pairs(i.has    ) do push(xys,{x=x,y=true} ) end
  for _,x in pairs(other.has) do push(xys,{x=x,y=false}) end
  merge(div(at,name,sort(xys,upx))) end

function div(at,name,xys)
  local now     = BIN(at, name, xys[1].x)
  local out     = {now}
  local minSize = #xys^the.leaves
  local epsilon = (per(xys,.9).x - per(xys,.1).x)/2.56
  for j,xy in pairs(xys) do
    if j > minSize and j + minSize < #xys then 
      if xy.x ~= xys[j+1].x then
        if now.hi - now.lo > epsilon then
           now = push(out, BIN(at, name, now.hi)) end end end
    now:add(xy.x, xy.y) end 
  out[1].lo    = -math.huge
  out[#out].hi =  math.huge
  return out end

function merge(b4,      j,tmp,n,a,b,merged)
  j, tmp, n = 1, {}, #b4
  while j<=n do
    a = b4[j]
    if j < n - 1 then
      b = b4[j+1]
      merged = a.ys:merged(b.ys) -- merge has to rereturn a new bin eith at name
      if merged then
        a = BIN(a.lo, b.hi,  merged)
        j = j+1 end end 
    tmp[#tmp+1] = a
    j = j+1 end
  return #tmp==#b4 and tmp or merge(tmp) end

return NUM
