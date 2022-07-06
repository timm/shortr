-- ## Class ROW
-- Hold one record (contained within [ROWS](rows.md)). 

-- **RESPONSIBILITIES** : 
-- - Sorting (on dependent columns) to find better ROWs (see `__lt`)
-- - Distance calculations (see `__sub`)
-- - Knows the data space that contains it (see `__of`).
-- - Knows its klass (see `klass`).
-- - Discretization (see `bin, merge, merges`)
-- - Distance calcs (see `dist, around, far`)

-- **COLLABORATORS** :
-- - [ROWS](rows.md) : the data space that contains it
-- ------------------------------------------------------------
local all = require"all"
local big,chat,lt,map  = all.big, all.chat, all.lt, all.map
local obj,rnds,sort    = all.obj, all.rnds, all.sort

-- ROW(of:ROWS, cells:tab) :ROW --> Place to store one record
-- (and stats on how it is used; e.g. `i.evaled=true` if we touch the y values.
local ROW = obj("ROW", function(i,of,cells) 
  i.cells  = cells -- :tab  -- the stored record
  i._of    = of    -- :ROWS -- back pointer to data space that contains this
  i.evaled = false -- :bool -- true if we ever use the dependent variables.
  end)

-- better(i:ROW, j:ROW):boolean --> should `i` proceed before `j`?
function ROW.__lt(i,j)
  i.evaled, j.evaled = true, true
  local s1, s2, ys = 0, 0, i._of.cols.y
  for _,col in pairs(ys) do
    local x,y =  i.cells[col.at], j.cells[col.at]
    x,y = col:norm(x), col:norm(y)
    s1  = s1 - 2.7183^(col.w * (x-y)/#ys)
    s2  = s2 - 2.7183^(col.w * (y-x)/#ys) end
  return s1/#ys < s2/#ys  end

-- i:ROW - j:ROW --> return distance between `i` and `j`
function ROW.__sub(i,j) 
  local d, cols = 0, i._of.cols.x
  for _,col in pairs(cols) do
    local inc = col:dist(i.cells[col.at], j.cells[col.at]) 
    d         = d + inc^the.p end
  return (d / #cols) ^ (1/the.p) end

-- around(i:ROW, rows:?[ROW]):tab -->  return rows in this table
-- sorted by distance to `i`. `rows` defaults to the rows of this ROWS.
function ROW.around(i, rows)
  local function rowGap(j) return {row=j, gap=i - j} end
  return sort(map(rows or i._of.rows, rowGap), lt"gap") end

-- far(i:ROW,rows:?[ROW]):ROW --> find something `far` away.
function ROW.far(i,rows) return per(i:around(rows), the.Far).row end

-- klass(i:ROW):any --> Return the class value of this record.
function ROW.klass(i) return i.cells[i._of.cols.klass.at] end

return ROW
