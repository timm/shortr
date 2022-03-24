local summary = require"summary"
local lib     = require"lib"
local map,sort,many = lib.map,lib.sort,lib.many
local items,slice   = lib.items,lib.slice

---     _ _ _  _ _|_ _ 
---    (_| (/_(_| | (/_
                
local egs={}
function egs.new() return {rows={}, cols={}} end

function egs.Init(data,    i)
  i= egs.new()
  for row in items(data) do
    if  #i.cols==0 then i.cols=summary.new(row) else 
      push(i.rows, summary.add(i.cols,row)) end end 
  return i end

---     egs      _  _  
---    (_| |_|(/_| \/
---      |/        / 

function egs.mid(i,cols)
   local function mid(col) return col.nump and col.mu or col.mode end
   return map(cols or i.cols.y, mid) end

function egs.div(i,cols)
   local function div(col) return col.nump and col.sd or ent(col.has) end
   return map(cols or i.cols.y, div) end

function egs.clone(old,rows)
  local i={rows={}, cols=summary.new(old.cols.names)}
  for key,row in pairs(rows or {}) do summary.add(i.cols,row) end
  return i end

---     _|. __|_ _  _  _ _ 
---    (_||_\ | (_|| |(_(/_
                    
function egs.dist(i,row1,row2)
  local function sym(c,x,y) return x==y and 0 or 1 end
  local function num(c,x,y)
    if     x=="?" then y = norm(c.lo, c.hi, y); x=y<.5 and 1 or 0 
    elseif y=="?" then x = norm(c.lo, c.hi, x); y=x<.5 and 1 or 0
    else             x,y = norm(c.lo, c.hi, x), norm(c.lo, c.hi, y) end
    return math.abs(x-y) end
  local function dist(c,x,y)
    return x=="?" and y=="?" and 1 or (c.nump and num or sym)(c,x,y) end
  local d, n = 0, #i.cols.x
  for key,c in pairs(i.cols.x) do d= d + dist(c, row1[c.at], row2[c.at])^the.e end 
  return (d/n)^(1/the.e) end

---     _ _  _ _|_ _ _  __|_ 
---    (_(_)| | | | (_|_\ | 
                       
function egs.bestRest(i)
  i.rows  = sort(i.rows, function(a,b) return summary.better(i.cols,a,b) end) 
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
