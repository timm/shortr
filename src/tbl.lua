-- vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro 
--------- --------- --------- --------- --------- --------- 

-- Tables store [Row](row.html)s, which are summarized
-- [Columns](columns.html).
-- Tables can be `fixed` or `frozen` (explained below).

require "lib"
local Object = require("object")
local Columns= require("columns")
local csv    = require("csv")
local Row    = require("row")
local Tbl    = {is="Tbl"}

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

-- Tables can be initialized from csv files
-- using the call `Tbl.new{file="xxx.csv"}`.
function Tbl.read(i,file) 
  for line in csv(file) do Tbl.add(i,line) end
  return i end

-- When new data is added to tables,
-- the first row define the column name and type (e.g. numeric
-- or symbolic) and the rest of the rows define the table data.
function Tbl.add(i, a)
  if #i.cols.all==0  -- i.e. no headers seen so far
  then Tbl.header(i,a) 
  else Tbl.row(i,a) end end

-- New headers update the column headers.
function Tbl.header(i,a) 
  Columns.add(i.cols,a) end

-- New data updates the column summaries
-- (unless the table is `fixed`) and
-- to the list of rows in the table
-- (unless the table is `frozen`).
function Tbl.row(i,a)
  if not i.frozen then
    for _,c in pairs(i.cols.all) do 
      c.me.add(c, a[c.pos]) end
  end
  if not i.fixed then
    i.rows[#i.rows + 1] = Row.new(a) end end  

-- New tables can be `cloned` from old ones
-- by (a) insert all the old column headers into
-- the new data then (b) optionally, adding in any
-- some old data.
function Tbl.clone(i, rows)
  i = Columns.clone(i.columns)
  if rows then 
    for _,row in pairs(rows) do 
      Tbl.row(i,row.cells) end end
  return i
end

function Tbl.class(i,row) return row.cells[i.cols.y.klass] end
-- ---------------------
-- And finally...

return Tbl
