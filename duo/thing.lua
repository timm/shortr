-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Thing = require("object"):extend("Thing")

function Thing:add(x)
  if x=="?" then return x end
  self.n = self.n + 1
  self:add1(x)
  return self
end

function Thing:sub(x)
  if x=="?"     then return x end
  if self.n < 1 then return x end
  self.n = self.n - 1
  self:sub1(x)
  return self
end

function Thing:adds(lst)
  for _,v in pairs(lst) do self:add(v) end
  return self
end  

function Thing:xpect(j)
  i = self
  local n = i.n + j.n
  return i.n/n * i:var() + j.n/n * j:var()
end

return Thing
