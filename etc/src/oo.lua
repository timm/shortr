fmt = string.format
function sort(t,f) table.sort(t,f); return t end
function cat(t, u) u={};for k,v in pairs(t) do u[1+#u]=tostring(k) end;return u;end

function obj(txt,base,  i,new)
  new = function(k,...) i=setmetatable({},k); k.new(i,...); return i end
  t={}; for k,v in pairs(base or {}) do t[k] = v end
  t.is, t.__index = txt, t
	return setmetatable(t,{__call=new}) end

_id = 0
local Object = obj"Object"
function Object:new() _id=_id+1; self._id=_id end

function Object:__tostring(   u,pub)
  pub = function(k,v) return tostring(k):sub(1,1)~="_" end
  u={}; for k,v in pairs(self) do if pub(k) then u[1+#u]=fmt(":%s %s",k,v) end end
  return (self.is or "").."{"..table.concat(sort(u)," ").."}" end

local Point = obj("Point",Object)
function Point:new(x,y) 
   Object.new(self)
   self.x,self.y= x,y end

function Point.__lt(self,other) return self.y < other.y end

local Pole = obj("Pole",Point)
function Pole:new(x,y,z)
  Point.new(self,x,y)
  self._w=100
  self.z = z end


p=Pole(10,120,5)
q=Pole(1,  20,6)
r=Pole(30,200,6)
t={p,q,r}
table.sort(t)
print(t[1],t[2],t[3])
