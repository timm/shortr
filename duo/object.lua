-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Object=require("30log")("Object")

function Object:init(t) 
  for k,v in self:has()      do self[k] = v end
  for k,v in pairs(t or {})  do self[k] = v end
  return self:inits()
end

function Object:has(t) return {} end

function Object.__tostring(self)
  local s, sep, lst, t = "", "", {}, self or {}
  for k,v in pairs(t) do
    if  k ~= "class" and k ~= "super" then
      if "_" ~= string.sub(k, 1, 1) then
        lst[#lst+1] = k end end end
  table.sort(lst)
  for k=1,#lst do
    s = s .. sep .. lst[k] .. "=" .. tostring(t[lst[k]]) 
    sep=", " 
  end 
  return (self.name or 'Object') .. "{" .. s .. "}"
end

return Object
