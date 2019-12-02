-- vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro 
--------- --------- --------- --------- --------- --------- 

local Object = require("Object")
local Row    = {is="Row"}

function Row.new(cells)
  local i = Object.new()
  i.me, i.cells, i.cooked = Row, cells, {}
  return i
end

-- ----------
-- And finally...


return Row
