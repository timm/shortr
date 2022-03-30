local ako,_   =require"ako", require"lib"
local obj,new,ent = _.obj, _.new ,  _.ent

local SYM = obj"SYM"

function SYM:new(at,name)   
  name = name or ""
  return new(SYM,{at=at or 0, name=name, 
            nump=false, indep=not ako.goal(name), 
            n=0, has={}, most=0, mode=nil}) end

function SYM:add(x)
  if x ~= "?" then
    self.n = self.n + 1
    self.has[x] = 1 + (self.has[x] or 0) 
    if self.has[x] > self.most then 
      self.mode,self.most = x,self.has[x] end end 
   return x end

return SYM
