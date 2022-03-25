local R = require
local the,seen,lib     = R"the", R"seen", R"lib"
local map,sort,up1     = lib.map,lib.sort,lib.up1
local items,push,slice = lib.items,lib.push,lib.slice
local any,many,cos     = lib.any, lib.many, lib.cosine
local o,oo,per,norm     = lib.o, lib.oo, lib.per, lib.norm

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
  for key,c in pairs(i.cols.x) do d=d+dist(c, row1[c.at], row2[c.at])^the.p end 
  return (d/n)^(1/the.p) end

function egs.neighbors(i, r1, rows)
  return sort(map(rows or i.rows,
              function(r2) return {egs.dist(i,r1,r2),r2} end), up1) end

function egs.half(i, rows)
  local project,far,some,left,right,c,lefts,rights,border
  rows    = rows or i.rows
  far     = function(r,t) return per(egs.neighbors(i,r,t), the.far)[2] end
  project = function(r)   
              return {cos(egs.dist(i,left,r), egs.dist(i,right,r),c),r} end
  some    = many(rows,     the.some)
  left    = far(any(some), some)
  right   = far(left,      some)
  c       = egs.dist(i,left,right)
  lefts,rights = egs.clone(i), egs.clone(i)
  for n, projection in pairs(sort(map(rows,project), up1)) do
    if n==#rows//2 then border = projection[1] end
    egs.add(n <= #rows//2 and lefts or rights, projection[2]) end
  return lefts, rights, left, right, border, c  end

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
