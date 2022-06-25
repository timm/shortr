-- ## class ROW:hold 1 record
local _=require"about"
local obj = _.obj

--> ROW(of:ROWS, cells:tab) :ROW -> Place to store one record
-- (and stats on how it is used; e.g. `i.evaled=true` if we touch the y values.
local ROW = obj("ROW", function(i,of,cells) 
  i._of,i.cells,i.evaled = of,cells,false end)

--> klass(i:ROW):any -> Return the class value of this record.
function ROW.klass(i) return i.cells[i._of.cols.klass.at] end

return ROW
