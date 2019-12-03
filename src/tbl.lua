-- vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro 
--------- --------- --------- --------- --------- --------- 

local Object = require("object")
local Columns= require("columns")
local Csv    = require("csv")
local Row    = require("row")
local Tbl    = {is="Tbl"}

function Tbl.new(t)
  local i = Object.new()
  i.me = Tbl
  i.rows, i.cols = {}, Columns.new()
  t = t or {}
  if t.file then Tbl.read(i,t.file) end
  i.frozen = t.frozen or false
  i.fixed = t.fixed or false
  return i
end

function Tbl.read(i,file) 
   Csv(file, function(a,   f) 
               f = #i.rows==0 and Tbl.header or Tbl.row
               f(i,a)
             end) 
end

function Tbl.header(i,a) Columns.add(i.cols,a) end

function Tbl.row(i,a)
  if not i.frozen then
    for _,x in pairs(i.cols.all) do c.me.add(c, a[c.pos]) end
  end
  if not i.fixed then
    i.rows[#i.rows + 1] = Row.new(a) 
  end
end  

function Tbl.clone(i, rows)
   i = Columns.clone(i.columns)
   if rows then 
     for _,row in pairs(rows) do Tbl.row(i,row.cells) end end
   return i
end
