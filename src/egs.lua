local R = require
local the,seen,lib     = R"the", R"seen", R"lib"
local map,sort,up1     = lib.map,lib.sort,lib.up1
local items,push,slice = lib.items,lib.push,lib.slice
local o,oo     = lib.sort
---     _ _ _  _ _|_ _ 
---    (_| (/_(_| | (/_
                
local egs={}
function egs.new() return {rows={}, cols=nil} end

function egs.Init(data,    i)
  i= egs.new()
  for row in items(data) do
    if  not i.cols then i.cols=seen.new(row) else egs.add(i,row) end end
  return i end

function egs.add(i,row)
  push(i.rows, seen.add(i.cols, row)) end 
---     _      _  _  
---    (_| |_|(/_| \/
---      |/        / 

function egs.mid(i,cols)
   local function mid(col) return col.nump and col.mu or col.mode end
   return map(cols or i.cols.y, mid) end

function egs.div(i,cols)
   local function div(col) return col.nump and col.sd or ent(col.has) end
   return map(cols or i.cols.y, div) end

function egs.clone(old,rows)
  local i={rows={}, cols=seen.new(old.cols.names)}
  for key,row in pairs(rows or {}) do seen.add(i.cols,row) end
  return i end
---     _ _  _ _|_ _ _  __|_ 
---    (_(_)| | | | (_|_\ | 
                       
function egs.bestRest(i)
  i.rows  = sort(i.rows, function(a,b) return seen.better(i.cols,a,b) end) 
  local n = (#i.rows)^the.best
  return slice(i.rows, 1,          n),      -- top n things
         many( i.rows, n*the.rest, n+1) end -- some sample of the rest

function egs.Contrasts(i, rows1, rows2)
  local function contrast(col)
    local function asBin(x,ys,     n,div)
      n,div = ent(ys)
      return bin.new(id, col.at, col.name, x, x, n, div) end
    local symbols, xys, x = {},{}
    for klass,rows in pairs{rows1,rows2} do
      for key,row in pairs(rows) do 
        x = row[col.at] 
        if x ~= "?" then 
          if not col.nump then inc2(symbols,x,klass) end
          push(xys, {x=x, y=klass}) end end end
    return col.nump and bins(xys, col.at) or collect(symbols, asBin) end
  local out, tmp = {}
  for key,col in pairs(i.cols.x) do
    tmp = contrast(col)
    if #tmp > 1 then
      for key,one in pairs(tmp) do push(out, one) end end end
   return out end

function egs.xplain(i)
  best, rest = egs.bestRest(i)
  return egs.contrasts(i, best,rest) end

return egs 
