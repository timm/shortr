-- ## Summarize data
local the=require"the"
local obj,per = _.obj,_.per

--- ## Adding
function ROWS.add(i,row)
  i.cols:add( push(i.rows, t.cells and t or ROW(i,row))) end

function COLS.add(i,row)
  for _,cols in pairs{i.x,i.y} do
    for _,col in pairs(cols) do col:add(row.cells[col.at]) end end end

function NUM.add(i,x,n)
  if x=="?" then return end
  n = n or 1
  for _=1,n  do
    if   #i.kept < i.nums   then i.ok=false; push(i.kept,x) 
    elseif R() < i.nums/i.n then i.ok=false; i.kept[R(#i.kept)]=x end end end 

function SYM.add(i,x,n)
  if x=="?" then return end
  i.ok = false
  i.kept[x] = n + (i.kept[x] or 0) end 

--- ## Querying
function Num.ok(i)
  if not i.ok then table.sort(i.kept) end
  i.ok = true
  return i.kelp end

function Num.mid(i) local a= i:ok(); return per(a,.5) end
function Sym.mid(i)
  local mode,most = nil,-1
  for x,n in pairs(i.kept) do if n > most then most, mode = n, x end end; return mode end

function Num.div(i) local a= i:ok(); return (per(a,.9)-per(a..1))/2.56 end
function Sym.div(i)
  local e,log=0, function(x) return math.log(x,2) end
  for x,n in pairs(i.kept) do if n > 0 then e=e- n/i.n*log(n/i.n) end end
  return e end

--- ### Column Factory
--------------------------------------------------------------------------------
local go,no={},{}

function go.CHAT() chat{aa=1,bb=3,cc={1,2,3}}; return true end

function go.ALL() 
  local fails,old = 0,{} 
  for k,v in pairs(the) do old[k]=v end
  for k,v in pairs(go) do
    if k~="ALL" then
      math.randomseed(the.seed or 10019)
      if v() ~= true then print("FAIL",k); fails=fails+1 end  
      for k,v in pairs(old) do the[k]=v end end end
  os.exit(fails) end


(go[arg[2]] or same)()  

-- local Rows=obj("Row", function(i,row) i.rows={}; i.cols=nil; i.categories={} end)
-- function Rows.add(i,row)
--   rs.kepts = rs.cols and maps(r.kepts,row,update) or i:categorize(kap(row,init) end)
--
-- function Rows.categorize(i,cols)
--   for _,col in pairs(cols) do if not col.ignorep then 
--      push(col.txt:find"[!+-]$" and i.categories.y or i.categories.y, col) end end 
--   return end
--
-- function make(f,rows) 
--   local function make1(row) if rows then rows:add(row) else rows=Rows(row) end
--   if type(src)=="table" then map(rows,make1) else csv(src,make1) end
--   return rows end
