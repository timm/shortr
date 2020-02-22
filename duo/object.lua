-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- ---------

local Object=require("30log")("Object")

function Object:init(t) 
  self:inits(t or {})
  return self
end

function Object:inits(t) 
  return self 
end

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
