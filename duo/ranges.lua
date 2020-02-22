-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Ranges=require("30log")("Object")

function Range:inits(t) 
  self.lst   = t.lst
  self.fx    = t.fx
  self.fy    = t.fy
  self.xs    = t.fx()
  self.ys    = t.fx()
  self.cohen = t.cohen or 0.2
  self.jump  = t.jump  or 0.5
  self.jump  = #(self.lst)^(t.jump)
  self.x     = t.x   or function (a) return a[1] end
  self.y     = t.y   or function (a) return a[#a] end
  table.sort(self.lst, (t.sort or 
                         function (a,b) 
		           return self.x(a) < self.x(b) end))
  self.first = self.x( self.lst[1] )
  self.last  = self.x( self.lst[ #(self.lst) ] )
  for _,v in pairs(self.lst) do
    xs:add( self.x(v) )
    ys:add( self.y(v) )
  end
  self.epsilon = t.epsilon or self.cohen * ys:var()
  self.trivial = t.trival  or 1.01
  return self:div(1, #(self.lst), xs, ys, {})
end

function Range:div(lo,hi, xrhs,yrhs,cuts,   
                   yrhs1, xlhs1, ylsh1, cut)
  local min, xlhs, ylhs = yrhs:var(), self.fx(), self.fy()
  for i=lo,hi-1 do
    local x     = self.x( self.lst[i]   )
    local xnext = self.x( self.lst[i+1] )
    local y     = self.y( self.lst[i]   )
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
       self.trivial < min / ylhs:xpect(yrhs)
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
