-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local copy=require("lib").copy

local Ranges=require("30log")("Object")
function Range:has() return {
  lst     = {},
  cohen   = 0.2, 
  jump    = 0.5, 
  trivial = 1.01,
  epsilon = nil,
  fx      = Num, 
  fy      = Num, 
  x       = function(a) return a[1]  end,
  y       = function(a) return a[#a] end,
  sort    = function (a,b) 
              return self.x(a) < self.x(b) end}
end

function Range:setup() 
  table.sort(self.lst, self.sort)
  self.first  = self.x( self.lst[1] )
  self.last   = self.x( self.lst[ #(self.lst) ] )
  self.jump   = #(self.lst)^(t.jump)
  local xs,ys = self.fx(), self.fy()
  for _,v in pairs(self.lst) do
    xs:add( self.x(v) )
    ys:add( self.y(v) )
  end
  self.epsilon = t.epsilon or self.cohen * ys:var()
  return self:div(1, #(self.lst), xs, ys, {})
end

function Range:div(lo,hi, xrhs,yrhs,cuts,   
                   yrhs1, xlhs1, ylsh1, cut)
  local min, xlhs, ylhs = yrhs:var(), self.fx(), self.fy()
  for i=lo,hi-1 do
    local y     = self.y( self.lst[i]   )
    local x     = self.x( self.lst[i]   )
    local xnext = self.x( self.lst[i+1] )
    xlhs:add(x)
    xrhs:sub(x)
    ylhs:add(y)
    yrhs:sub(y)
    if x ~= "?"   and
       x ~= xnext and
       i            > self.first + self.jump  and
       i            < self.last  - self.jump  and
       self.epsilon < self.last  - x          and
       self.epsilon < xnext      - self.first and
       self.epsilon < xrhs:mid() - xlhs:mid() and
       self.trivial < xrhs:mid() / xlhs:mid() and
       self.trivial < min        / ylhs:xpect(yrhs)
    then
       cut, min     = i, ylhs:xpect(yrhs)
       xrhs1, xlhs1 = copy(xrhs), copy(xlhs)
       yrhs1, ylhs1 = copy(yrhs), copy(ylhs)
    end 
  end
  if    cut 
  then  div(lo,    cut, xrhs1, yrhs1, cuts)
        div(cut+1, hi,  xlhs1, ylhs1, cuts)
  else  cuts[#(cuts)+1] = self.x( self.lst[lo] )
  end
  return cuts
end

return Range
