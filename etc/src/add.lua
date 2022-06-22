-- ## Update
local _=require"about"
local kl=require"klass"
local R,csv,push = _.R, _.csv, _.push
local COLS,NUM,SOME,SYM = kl.COLS, kl.NUM, kl.SOME, kl.SYM
local ROW,ROWS          = kl.ROW, kl.ROWS

--> add(i:SOME: x:num)-> `n` times,update `i`.
-- If full then at odss `i.some/i.x`, keep `x`, replace anything a random.
function SOME.add(i,x)
  if x ~= "?" then 
    i.n = i.n + 1
    if #i.kept < i.max     then i.ok=false; push(i.kept,x) 
    elseif R() < i.max/i.n then i.ok=false; i.kept[R(#i.kept)]=x end end end 

--> add(i:NUM: x:num, n:?int=1) -> `n` times,update `i`.
function NUM.add(i,x,n)
  if x ~="? " then 
    for _ = 1,(n or 1) do i.n=i.n+1; i.kept:add(x) end end end

--> add(i:SYM: x:sum, n:?int=1) -> Add `n` count to `i.kept[n]` .
function SYM.add(i,x,n)
  if x ~= "?" then 
    i.kept[x] = (n or 1) + (i.kept[x] or 0) end end

--> add(i:COLS: row:ROW) -> Update columns using data from `row`.
function COLS.add(i,row)
  for _,cols in pairs{i.x,i.y} do
    for _,col in pairs(cols) do col:add(row.cells[col.at]) end end end

--> adds(i:ROWS: src:(str|tab)):ROWS -> copy the data from `src` into `i`.
function ROWS.adds(i,src)
  (type(src)=="table" and map or csv)(src, function(t) i:add(t) end) 
  return i end

--> add(i:ROWS: row:ROW) -> add ROW to ROWS, update the summaries in `i.cols`.
function ROWS.add(i,t) 
  t = t.cells and t or ROW(i,t)
  if i.cols then i.cols:add(push(i.rows, t)) else i.cols=COLS(t.cells) end end

