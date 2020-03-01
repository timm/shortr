-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Data = require("object"):extend("Data")
local csv  = require("csv")

function Data:has() return {
  rows   = {},  
  cols   = {},
  file   = "",
  keepCols = true,
  keepRows = true}
end

function Data:make()
  if #self.file > 0 then
    for line in csv(self.file) do self:add(line) end 
  end
end                                                                                                                         
function Data:add(a) 
  if #self.col > 0 then
    ifs  #self.cols.all == 0 and self:header(a) or self:row(a)
end

function Data:header(a)
  self.cols = Columns{names=a}
end

function Data:row(a) 
  if self.keepCols then
    for _,c in pairs(self.cols.all) do c:add(a[c.pos]) end
  end
  if self.wantRows then 
    self.rows[#self.rows+1] = Row(a) 
  end
end

function Data:clone(rows)
  local tmp = self.cols:clone()
  for _,row in pairs(rows or {}) do
    tmp:row(row.cells) end
  return tmp
end

return Data
