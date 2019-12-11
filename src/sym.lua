-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

local Column = require("column")
local THE   = require("the")
local Sym   = {is="Sym"}

function Sym.var(i) return i.sd end
function Sym.mid(i) return i.mode end
function Sym.xpect(i,j,  n)  
  n = i.n + j.n +0.0001
  return i.n/n * Sym.ent(i) + j.n/n * Sym.ent(j) 
end

function Sym.new(t)
  local i  = Column.new(t)
  i.me     = Sym
  i.counts = {}
  i.most   = 0
  i.mode   = nil
  i.ent   = nil
  return i
end

function Sym.ent(i)
  if i.ent == nil then
    i.ent = 0
    for k,v in pairs(i.counts) do
      local p = v/i.n
      i.ent = i.ent - p*math.log(p,2) end end
  return i.ent
end

-- Bulk addition to a `Num`.
function Sym.alls(lst,key, i)
  i = Sym.new{key=key}
  for _,x in pairs(lst) do Sym.add(i, x) end
  return i
end

-- Add one item to a `Num`.
function Sym.add(i,x,   d)
  if x=="?" then return x end
  x = i.key(x)
  i.n = i.n + 1
  i.ent= nil
  d = (i.counts[x] or 0) + 1
  i.counts[x] = d
  if d > i.most then i.most, i.mode = d, x end
end

function Sym.dec(i,x,   d)
  if x=="?" then return x end
  x = i.key(x)
  i.ent= nil
  if i.n > 0 then
    i.n = i.n - 1
    i.counts[x] = i.counts[x] - 1 end
end

function Sym.xpect(i,j,   n)  
  n = i.n + j.n + 0.0001
  return i.n/n * Sym.ent(i) + j.n/n * Sym.ent(j)
end

-- --------
-- Distance between rows
function Sym.dist(i,x,y)
  if   x == THE.char.skip and y == THE.char.skip then return 1
  else return  x==y  and 0 or 1
  end
end
  
-- ----------
-- And finally...


return Sym
