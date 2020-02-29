-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Object=require("30log")("Object")

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

function Object:make() return self end
function Object:has()   return {}   end

function Object.__tostring(self)
  local s, sep, lst, t = "", "", {}, self or {}
  local f = function(z)  
    if type(z)=="function" then return "FUNC" end
    if type(z)=="table"  and  not z.class then return "TABLE" end
    return tostring(z)  end
  for k,v in pairs(t) do
    if  k ~= "class" and k ~= "super" then
      if "_" ~= string.sub(k, 1, 1) then
        lst[#lst+1] = k end end end
  table.sort(lst)
  for k=1,#lst do
    s = s .. sep .. lst[k] .. "=" .. f(t[lst[k]]) 
    sep=", " 
  end 
  local pre = (t and t.class and t.class.name) or "Object"
  return pre  .. "{" .. s .. "}"
end

return Object
