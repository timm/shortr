-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Row = require("object"):extend("Row")

function Row:has(s) return {
  cells={},
  cooled={}}
end

return Row
