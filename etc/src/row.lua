-- ## class ROW:hold 1 record
local all = require"all"
local lt,map,obj,sort = all.lt, all.map, all.obj, all.sort

--> ROW(of:ROWS, cells:tab) :ROW -> Place to store one record
-- (and stats on how it is used; e.g. `i.evaled=true` if we touch the y values.
local ROW = obj("ROW", function(i,of,cells) 
  i._of,i.cells,i.evaled = of,cells,false end)

--> i:ROW - j:ROW -> return distance between `i` and `j`
function ROW.__lt(i,j) 
  local d, cols = 0, i._of.cols.x
  for _,col in pairs(cols) do
    local inc = col:dist(i.cells[col.at], j.cells[col.at]) 
    d         = d + inc^the.p end
  return (d / #cols) ^ (1/the.p) end

--> around(i:ROW, rows:?[ROW]):tab ->  return rows in this table
-- sorted by distance to `i`. `rows` defaults to the rows of this ROWS.
function ROW.around(i, rows)
  local function rowGap(j) return {row=j, gap=i - j} end
  return sort(map(rows or i._of.rows, rowGap), lt"gap") end

--> better(i:ROW, j:ROW):boolean -> should `i` proceed before `j`?
function ROW.better(i,j)
  i.evaled, j.evaled = true, true
  local s1, s2, ys = 0, 0, i._of.cols.y
  for _,col in pairs(ys) do
    local x,y =  i.cells[c.at], j.cells[c.at]
    x,y = col:norm(x), col:norm(y)
    s1  = s1 - 2.7183^(col.w * (x-y)/#ys)
    s2  = s2 - 2.7183^(col.w * (y-x)/#ys) end
  return s1/#ys < s2/#ys  end

--> far(i:ROW,rows:?[ROW]):ROW -> find something `far` away.
function ROW.far(i,rows) return per(Row.around(i,rows), the.Far).row end

--> klass(i:ROW):any -> Return the class value of this record.
function ROW.klass(i) return i.cells[i._of.cols.klass.at] end

return ROW
