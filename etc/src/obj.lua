function cat(t,  u)
  u={}; for k,v in pairs(t) do u[1+#u]=string.format(":%s %s",k,v) or v end
  if #t==0 then table.sort(u) end 
  return "{"..table.concat(u," ").."}" end

function obj(txt,   t,i) 
  local function new(k,...) 
    local i=setmetatable({},k)
    local tmp=k.new(i,...)
    return tmp and setmetatable(tmp,k) or i end
  t={__tostring = function(x) return txt..cat(x) end}
  t.__index = t;return setmetatable(t,{__call=new}) end

Emp=obj"Emp"

function Emp:new(x) return {x=x,n=0} end
function Emp:add(y) self.x = self.x + y end

e=Emp(10)
e:add(100)
print(e)
