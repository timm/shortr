-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 
 
local THE  = require("lib")
local Some = require("some")
local Tree = {is="Tree"}

function Tree.new(t)
  local i = Object.new()
  i.me, i.tbl  = Tree,t
  return i
end

function Tree.ranges(i, s)
  for _,col in pairs(i.tbl.cols.nums) do
    s=Some.new()
    for _,row in pairs(t.rows) do
      Some.add(s, row.cells[col.pos]) end
    d= Some.divs(s)
    for _,row in pairs(t.rows) do
      row.cooked[col
    end
  end
end}

