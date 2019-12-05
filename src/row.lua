-- vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro 
--------- --------- --------- --------- --------- --------- 

-- <center><img 
--  src="https://github.com/timm/lua/raw/master/etc/img/row.jpg"></center>

local Object = require("Object")
local Lib    = require("lib")
local THE    = require("the")
local Row    = {is="Row"}

function Row.new(cells)
  local i = Object.new()
  i.me, i.cells, i.cooked = Row, cells, {}
  return i
end

-- Distance between rows
function Row.dist(i,j,t,p,cols,     x,y,d1,d,n)
  d, n, p = 0, 0.0001, p or THE.dist.p
  for _,c in pairs(cols or t.cols.x.all) do
    x,y = i.cells[c.pos], j.cells[c.pos]
    d1  = c.me.dist(c, x, y)
    d   = d + d1^p
    n   = n + 1
  end
  return ( d/n ) ^ ( 1/p )
end

-- k-th nearest neighbors
function Row.knn(i,k,get,combine,t,rows,p,cols,    a)
  get = get or function(z) 
         return z.cells[t.cols.y.klass.pos] end
  a= Row.neighbors(i, t,rows,p,cols)
  b={}
  for j=1,k do b[#b+1] = get(a[j]) end
  combine = combine or mean
  return combine(b)
end

-- Return  `rows`, sorted by distance to row `i`.
function Row.neighbors(i,t,rows,p,cols,   a)
  a = {}
  for _,j in pairs(rows or t.rows) do 
    a[#a+1]= {Row.dist(i,j,t,p,cols), j} 
  end
  table.sort(a, function (x,y) return x[1] < y[1] end)
  return a
end

-- ----------
-- And finally...

return Row
