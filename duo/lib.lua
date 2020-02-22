-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Lib={}

function Lib.list() return {} end

function Lib.map(a,f,     b)
  b = {}
  f = f or function(z) return z end
  for i,v in pairs(a or {}) do b[i] = f(v) end 
  return b
end 

function Lib.copy(t) 
  return type(t) ~= 'table' and t or Lib.map(t,Lib.copy) 
end

return Lib
