-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

local Column = require("column")
local Sym   = {is="Sym"}

function Sym.new(t)
  local i  = Column.new(t)
  i.me     = Sym
  i.counts = {}
  i.most   = 0
  i.mode   = nil
  i.ent    = nil
  return t
end

function Sym.ent(i)
  if i.ent == nil then
    for k,v in pairs(i.count) do
      p = v/i.n
      i.ent = i.ent - p*math.log(p,2) end end
  return i.ent
end

function Sym.add(i,x,    d)
  if x=="?" then return x end
  x = i.key(x)
  t._ent= nil
  d = (t.counts[x] or 0) + 1
  t.counts[x] = d
  if d > t.most then
    t.most, t.mode = d, x end
end

function Sym.dec(i,x,   d)
  if x=="?" then return x end
  x = i.key(x)
  t._ent= nil
  if t.n > 0 then
    t.n = t.n - 1
    t.counts[x] = t.counts[x] - 1 end
end

function Sym.xpect(i,j,   n)  
  n = i.n + j.n + 0.0001
  return i.n/n * Sym.ent(i) + j.n/n * Sym.ent(j)
end

return Sym
