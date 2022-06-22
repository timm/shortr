local _=require"about"
local kl=require"klass"
local R,push = _.R,_.push
local COLS,NUM,SOME,SYM,ROWS = kl.COLS, kl.NUM, kl.SOME, kl.SYM, kl.ROWS

--> add(i:SOME: x:num)-> `n` times,update `i`.
-- If full then at odss `i.max/i.x`, keep `x`, replace anything a random.
function SOME.add(i,x)
  if x ~= "?" then 
    i.n = i.n + 1
    if   #i.kept < i.some   then i.ok=false; push(i.kept,x) 
    elseif R() < i.some/i.n then i.ok=false; i.kept[R(#i.kept)]=x end end end 

--> add(i:NUM: x:num, n:?int=1) -> `n` times,update `i`.
function NUM.add(i,x,n)
  if x ~="? " then 
    for _ = 1,(n or 1) do i.n=i.n+1; i.some:add(x) end end end

--> add(i:SYM: x:sum, n:?int=1) -> Add `n` count to `i.kept[n]` .
function SYM.add(i,x,n)
  if x ~= "?" then 
    i.kept[x] = n + (i.kept[x] or 0) end end

--> add(i:COLS: row:ROW) -> Update columns using data from `row`.
function COLS.add(i,row)
  for _,cols in pairs{i.x,i.y} do
    for _,col in pairs(cols) do col:add(row.cells[col.at]) end end end

-- -> adds(i:ROWS: src:(str|tab)):ROWS -> copy the data from `src` into `i`.
function ROWS.adds(i,src)
  if     type(src)=="table" 
  then   map(src, function(row) i:add( row      ) end) 
  else   csv(src, function(t)   i:add( Row(i,t) ) end) end
  return i end

--> add(i:ROWS: row:ROW) -> add ROW to ROWS, update the summaries in `i.cols`.
function ROWS.add(i,row) 
  if i.cols then i.cols:add(push(i.rows, row)) else i.cols=COLS(row) end end

