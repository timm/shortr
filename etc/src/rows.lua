-- ## class ROWS: store many ROW
local all = require"all"
local csv,map,obj       = all.csv, all.map,  all.obj
local push,rnd,rnds,the = all.push, all.rnd, all.rnds, all.the
local COLS,ROW          = require"COLS",require"ROW"

--> ROWS(names:?[str], rows:?[ROW}) :ROWS -> Place to store many ROWS
--  and summarize them (in `i.cols`).
local ROWS = obj("ROWS", function(i,names,rows) 
  i.rows, i.cols = {}, (names and COLS(names) or nil)
  for _,row in pairs(rows or {}) do i:summarize(row) end end)

--> add(i:ROWS: row:ROW) -> add ROW to ROWS, update the summaries in `i.cols`.
function ROWS.add(i,t) 
  t = t.cells and t or ROW(i,t)
  if i.cols then i.cols:add(push(i.rows, t)) else i.cols=COLS(t.cells) end end

--> ROWS.clone(init:?[ROW]) :ROWS -> Return a ROWS with same structure as `i`. 
-- Optionally, `init`ialize it with some rows. Add a pointer back to the 
-- original table that spawned `eve`rything else (useful for some distance calcs).
function ROWS.clone(i,init)
  local j=ROWS(i.cols.names,init)
  return j end

--> fill(i:ROWS: src:(str|tab)):ROWS -> copy the data from `src` into `i`.
function ROWS.fill(i,src)
  local what2do = type(src)=="table" and map or csv
  what2do(src, function(t) i:add(t) end) 
  return i end

--> like(i:ROWS,row;ROW,nklasses:num,nrows:num):num -> Return 
-- P(H)*&prod;<sub>i</sub> (P(E<sub>i</sub>|H)). Do it with logs
-- to handle very small numbers.
function ROWS.like(i,row, nklasses, nrows)
  local prior,like,inc,x
  prior = (#i.rows + the.k) / (nrows + the.k * nklasses)
  like  = math.log(prior)
  row   = row.cells and row.cells or row
  for _,col in pairs(i.cols.x) do
    x = row[col.at]
    if x and x ~= "?" then
      inc  = col:like(x,prior)
      like = like + math.log(inc) end end
  return like end

--> mids(i:ROW,p:?int=2,cols=?[COL]=i.cols.y):tab -> Return `mid` of columnss
-- rounded to `p` places.
function ROWS.mids(i,p,cols,    t) 
  t={}
  for _,col in pairs(cols or i.cols.y) do t[col.txt]=col:mid(p) end
  return rnds(t,p or 2) end

return ROWS
