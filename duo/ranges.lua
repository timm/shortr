-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Ranges= require("thing"):extend("Ranges")
local copy  = require("lib").copy
local Num   = require("num")
local Sym   = require("sym")

function Ranges:has() return {
  lst     = {},
  cuts    = {},
  cohen   = 0.2, 
  jump    = 0.5, 
  trivial = 1.01,
  epsilon = 0,
  fx      = Num, 
  fy      = Num, 
  x       = function(a) return a[1]  end,
  y       = function(a) return a[#a] end,
  sort    = function (a,b) 
              return self.x(a) < self.x(b) end}
end

function Ranges:make() 
  table.sort(self.lst, self.sort)
  self.first  = self.x( self.lst[1] )
  self.last   = self.x( self.lst[ #(self.lst) ] )
  self.jump   = (#self.lst)^self.jump
  local xs,ys = self.fx(), self.fy()
  for _,v in pairs(self.lst) do
    xs:add( self.x(v) )
    ys:add( self.y(v) )
  end
  self.epsilon = self.epsilon > 0 and self.epsilon 
                 or self.cohen * ys:var()
  self:div(1, #(self.lst), xs, ys)
end

function Ranges:div(lo,hi, xrhs,yrhs,   
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
  then  div(lo,    cut, xrhs1, yrhs1)
        div(cut+1, hi,  xlhs1, ylhs1)
  else  for i=lo,hi do
          self.cuts[#(self.cuts)+1] = self.x( self.lst[lo] )
        end
  end
end

return Ranges
