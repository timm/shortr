-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 
-- [home](index.html) :: [about](about.html) :: [github](http://github.com/timm/lua) :: [discuss](http://github.com/timm/lua/issues) :: [&copy; 2020](https://github.com/timm/lua/blob/master/LICENSE.md) by [timm](http://menzies.us)


local Column = require("column")
local Sym   = {is="Sym"}

function Sym.new(t)
  local i  = Column.new(t)
  i.me     = Sym
  i.counts = {}
  i.most   = 0
  i.mode   = nil
  i._ent    = nil
  return t
end

function Sym.ent(i)
  if i._ent == nil then
    for k,v in pairs(i.count) do
      p = v/i.n
      i._ent = i._ent - p*math.log(p,2) end end
  return i._ent
end

function Sym.add(i,x,    d)
  if x=="?" then return x end
  x = i.key(x)
  i._ent= nil
  d = (i.counts[x] or 0) + 1
  i.counts[x] = d
  if d > i.most then
    i.most, i.mode = d, x end
end

function Sym.dec(i,x,   d)
  if x=="?" then return x end
  x = i.key(x)
  i._ent= nil
  if i.n > 0 then
    i.n = i.n - 1
    i.counts[x] = i.counts[x] - 1 end
end

function Sym.xpect(i,j,   n)  
  n = i.n + j.n + 0.0001
  return i.n/n * Sym.ent(i) + j.n/n * Sym.ent(j)
end

return Sym
