local ako,_,COL   =require"ako", require"lib", require"COL"
local obj,new,ent = _.obj, _.new ,  _.ent
local SYM = class("SYM",COL)

function SYM:new(at,name)   
  self:super(at,name)
  self.has, self.most, self.mode = {}, 0, nil end

function SYM:add1(x)
  self.has[x] = 1 + (self.has[x] or 0) 
  if self.has[x] > self.most then 
    self.mode, self.most = x, self.has[x] end end 

function SYM:div()      return ent(i.has) end
function SYM:mid()      return i.mode end
function SYM:same(x,y)  return x==y end

function SYM:dist1(x,y) 
  return self:same(x,y)  and 0 or 1 end

function SYM:like1(x,prior)
  return ((i.has[x] or 0) + the.M*prior)/(self.n + the.M) end

function SYM:merge(other,      out)
  out = SYM:new(self.at, self.name)
  for x,n in pairs(self.has)  do out[x] = n+(out[x] or 0) end
  for x,n in pairs(other.has) do out[x] = n+(out[x] or 0) end
  return out end

return SYM
