-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Object=require("30log")("Object")

local oo = require("lib").oo

function Object:init(t) 
  local want = self:has()
  for k,v in pairs(want) do self[k] = v end
  for k,v in pairs(t or {})  do 
     assert(want[k],"bad key ["..tostring(k).."]")
     self[k] = v 
  end
  self:make()
  return self
end

function Object:has()            return {} end
function Object:make()           return self end
function Object.__tostring(self) return oo(self) end

return Object
