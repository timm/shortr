-- ## Class ROWS
--  ROWS stores (and makes summaries  of )    ROWs.

-- **RESPONSIBILITIES** : 
-- - Store many ROWs
-- - Load csv files into ROWS (see `fill`)
-- - Summarize ROWs in NUM or SYM columns (see `add`)
-- - Report summaries (see `mid`)
-- - Clone (create new ROWS with the same structure) (see `clone`)
-- - Distance calculations (see `dist`)
-- - Bayesian likelihood calculations (see `add`)   

-- **COLLABORATORS** :
-- - ROW, COLS (and COLS are factories  that decide what NUMs or SYMs to make).
-- ------------------------------------------------------------
local all = require"all"
local any,chat,csv,lt,many,map,obj  = all.any,all.chat, all.csv, all.lt,all.many, all.map, all.obj
local push,rnd,rnds,small,sort,the = all.push, all.rnd, all.rnds, all.small,all.sort,all.the
local COLS,ROW          = require"COLS",require"ROW"

-- ### Create
-- ROWS(names:?[str], rows:?[ROW}) :ROWS --> Place to store many ROWS
--  and summarize them (in `i.cols`).
local ROWS = obj("ROWS", function(i,names,rows) 
  i.rows, i.cols = {}, (names and COLS(names) or nil)
  for _,row in pairs(rows or {}) do i:add(row) end end)

-- ROWS.clone(init:?[ROW]) :ROWS --> Return a ROWS with same structure as `i`. 
-- Optionally, `init`ialize it with some rows. Add a pointer back to the 
-- original table that spawned `eve`rything else (useful for some distance calcs).
function ROWS.clone(i,init)
  local j=ROWS(i.cols.names,init)
  return j end

-- fill(i:ROWS: src:(str|tab)):ROWS --> copy the data from `src` into `i`.
function ROWS.fill(i,src)
  local iterate = type(src)=="table" and map or csv
  iterate(src, function(t) i:add(t) end) 
  return i end

-- ### Likelihood
-- like(i:ROWS,row;ROW,nklasses:num,nrows:num):num --> Return -- P(H)*&prod;<sub>i</sub> (P(E<sub>i</sub>|H)). 
-- Do it with logs to handle very small numbers.
function ROWS.like(i,row, nklasses, nrows)
  local prior,like,inc,x
  prior = (#i.rows + the.k) / (nrows + the.k * nklasses)
  like  = math.log(prior)
  row = row.cells and row.cells or row
  for _,col in pairs(i.cols.x) do
    x = row[col.at]
    if x ~= nil and x ~= "?" then
      inc  = col:like(x,prior)
      like = like + math.log(inc) end end
  return like end

-- ### Cluster
function ROWS.best(i, rows, stop, rests)
  rows = rows or i.rows
  stop = stop or 2*small(the.Min,#rows)
  rests= rests or {}
  if #rows <= stop then return rows,rests end
  local xy = i:half(rows,stop,x)
  if xy.y < xy.x then xy.xs, xy.ys, xy.x, xy.y  = xy.ys, xy.xs, xy.y, xy.x end
  for _,row in pairs(xy.ys) do push(rests,row) end
  return i:best(xy.xs, stop, rests) end

function ROWS.half(i,rows,stop,x)
  rows = rows or i.rows
  stop = stop or 2*small(the.Min,#rows)
  local some    = many(rows,the.Some)
  x             = x or any(some):far(some)
  local y       = x:far(some)
  local c       = x - y
  local project = function(r) return {r=r, x=((r-x)^2+c^2-(r-y)^2)/(2*c)} end 
  local rxs     = map(rows, project)
  local xs,ys   = {},{} 
  for j,rx in pairs(sort(rxs, lt"x")) do push(j<=#rows*.5 and xs or ys, rx.r) end
  return {xs=xs, ys=ys, x=x, y=y, c=c} end

-- ### Report
-- mids(i:ROW,p:?int=2,cols=?[COL]=i.cols.y):tab --> Return `mid` of columns rounded to `p` places.
function ROWS.mids(i,p,cols) 
  local t={n=#i.rows}
  for _,col in pairs(cols or i.cols.y) do t[col.txt]=col:mid(p) end
  return rnds(t,p or 2) end

-- ### Update
-- add(i:ROWS: row:ROW) --> add ROW to ROWS, update the summaries in `i.cols`.
function ROWS.add(i,t) 
  t = t.cells and t or ROW(i,t)
  if i.cols then i.cols:add(push(i.rows, t)) else i.cols=COLS(t.cells) end 
  return t end

-- That's all folks.
return ROWS
