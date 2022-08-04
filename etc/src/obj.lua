function obj(sName,     new,self,t)
 function new(k,...)
   self = setmetatable({},k)
   return setmetatable(k.new(self,...) or self,k) 
  end ----------------- 
  t={_is=sName, __tostring=function(x) return cat(x) end}
  t.__index = t
  return setmetatable(t,{__call=new}) end

Emp=obj"Emp"

function Emp:new(x) return {x=x,n=0,_sectret=12} end
function Emp:add(y) self.x = self.x + y end

function cat(t,  u)
  u={}; for k,v in pairs(t) do 
  if tostring(k):sub(1,1) ~= "_" then
    u[1+#u]=string.format(":%s %s",k,v) or v end end
  if #t==0 then table.sort(u) end 
  return (t._is or "").. "{"..table.concat(u," ").."}" end

e=Emp(10)
e:add(100)
print(e)
