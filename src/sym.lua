local ako,_   =require"ako", require"lib"
local obj,new,ent = _.obj, _.new ,  _.ent

local SYM = obj"SYM"

function SYM:new(at,name)   
  name = name or ""
  return new(SYM,{at=at or 0, name=name, 
            nump=false, indep=not ako.goal(name), 
            n=0, has={}, most=0, mode=nil}) end

function SYM:add(x,inc)
  if x ~= "?" then
    inc = inc or 1
    self.n = self.n + inc
    self.has[x] = inc + (self.has[x] or 0) 
    if self.has[x] > self.most then 
      self.mode, self.most = x, self.has[x] end end 
   return x end

function SYM:div() return ent(i.has) end
function SYM:mid() return i.mode end

function SYM.merged(i,j,      k)
  k = SYM:new(i.at, i.name)
  for x,n in pairs(i.has) do k:add(x,n) end
  for x,n in pairs(j.has) do k:add(x,n) end
  if ent(k.has) * .95 <= (i.n*ent(i.has) + j.n*ent(j.has))/k.n then
    return k end end

return SYM
