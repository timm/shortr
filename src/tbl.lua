-- vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro 
--------- --------- --------- --------- --------- --------- 

local Object = require("object")
local Columns= require("columns")
local Csv    = require("csv")
local Row    = require("row")
local Tbl    = {is="Tbl"}

require "lib"

function Tbl.new(t)
  local i = Object.new()
  i.me = Tbl
  i.rows, i.cols = {}, Columns.new()
  t = t or {}
  i.frozen = t.frozen or false
  i.fixed  = t.fixed or false
  if t.file then Tbl.read(i,t.file) end
  return i
end

function Tbl.read(i,file) 
  Csv(file, function(a) Tbl.add(i,a) end) 
  return i end

function Tbl.add(i, a)
  if #i.cols.all==0 
  then Tbl.header(i,a) 
  else Tbl.row(i,a) end end

function Tbl.header(i,a) 
  Columns.add(i.cols,a) end

function Tbl.row(i,a)
  if not i.frozen then
    for _,c in pairs(i.cols.all) do 
      c.me.add(c, a[c.pos]) end
  end
  if not i.fixed then
    i.rows[#i.rows + 1] = Row.new(a) end end  

function Tbl.clone(i, rows)
   i = Columns.clone(i.columns)
   if rows then 
     for _,row in pairs(rows) do Tbl.row(i,row.cells) end end
   return i
end

return Tbl
