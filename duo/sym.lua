-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local list = require("lib").list
local Sym  = require("thing"):extend()

function Sym:has(t) return {
  counts = list(),
  most   = 0,
  mode   = '', 
  ent    = 0,
  n=0, pos=0, txt=0, w=1 }
end

function Sym:mid() return self.mu    end
function Sym:var() return self.ent() end

function Sym:add1(x)
  self.ent= nil
  local d = (self.counts[x] or 0) + 1
  self.counts[x] = d
  if d > self.most then self.most, self.mode = d, x end
end

function Sym:sub1(x)
  self.ent = nil
  if self.n > 0 then
    self.n = self.n - 1
    self.counts[x] = self.counts[x] - 1 end
end

function Sym.ent0(i)
  if i.ent == nil then
    i.ent = 0
    for k,v in pairs(i.counts) do
      local p = v/i.n
      i.ent = i.ent - p*math.log(p,2) end end
  return i.ent
end


return Sym
