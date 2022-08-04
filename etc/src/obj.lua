function cat(t,  u)
  u={}; for k,v in pairs(t) do 
  if tostring(k):sub(1,1) ~= "_" then
    u[1+#u]=string.format(":%s %s",k,v) or v end end
  if #t==0 then table.sort(u) end 
  return (t._is or "").. "{"..table.concat(u," ").."}" end

function obj(sName)
  local t={__tostring = cat}
  t.__index = t
  return setmetatable(t,{__call=function(k,...)
                                  local tmp=setmetatable({},k)
                                  local out=k.new(tmp,...) or tmp
                                  out._is = sName
                                  return setmetatable(out,k) end}) end

Emp=obj"Emp"

function Emp:new(x) return {x=x,n=0,_sectret=12} end
function Emp:add(y) self.x = self.x + y end

e=Emp(10)
e:add(100)
print(e)
