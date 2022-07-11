fmt=string.format
function cat(t,    u,pub)
  function pub(k,v) 
    if (tostring(k)):sub(1,1) ~= "_" then return fmt(":%s %s",k,cat(v)) end end
  if type(t) ~= "table" then return tostring(t) end
  u = {}
  if   #t>1 
  then for k,v in pairs(t) do u[1+#u]=cat(v) end
  else for k,v in pairs(t) do u[1+#u]=pub(k,v) end 
       table.sort(u) end
  return (t._is or "").."{"..table.concat(u," ").."}" end

function obj(txt,base,   new,i)
	function new(class,...) i=setmetatable({},class);class.new(i,...);return i end
  t={}
  for k,v in pairs(base or {}) do t[k] = v end
  t.__tostring, t._is, t._index = cat,txt,t
	return setmetatable(t,{__call=new}) end

local Point = obj"Point"
function Point:new(x,y) self.x,self.y= x,y end
function Point.__lt(self,other) return self.y > other.y end

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
