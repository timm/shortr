--vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

local Object = {is="Object"}

local n= 0
local function id() n=n+1; return n end

function Object.new()
  local i = {} 
  i.id, i.me = id(), Object
  return i
end

return Object
