-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 
 
-- This file returns a function that
-- divides a list of numbers such that the variability
-- of each division is minimized. 

local THE  = require("the")
local Lib  = require("lib")
local Object = require("object")
local Num  = require("num")
local Sym  = require("Sym")
local Divs = {is="Divs"}

o,r,copy,same = Lib.o, Lib.r, Lib.copy,Lib.same

function Divs.new(a, t) -- col,depth,x,xis,y,ys``
  local i = Object.new()
  i.me    = Divs
  t       = t or {}
  i.THE   = copy(t.the or THE.divs)
  i.xis   = t.xis or Num
  i.x     = t.x   or same
  i.yis   = t.yis or i.xis
  i.y     = t.y   or i.x
  a       = Divs.sort(i,a, i.THE.skip, t.most or i.THE.most)
  i.start = i.x(a[1])
  i.stop  = i.x(a[#a])
  i.step  = math.floor(#a)^i.THE.step
  return Divs.split(i, a, 1, #a, {}, t.depth or 1000)
end 

function Divs.sort(i,a,skip,most,    b)
  b={}
  for _,one in pairs(a) do
    if r() < most/#a then
      if i.x(one) ~= skip then
        b[#b+1] = one end end end
  table.sort(b, function(u,v) 
     return i.x(u) < i.x(v) end)
  return b
end

function Divs.split(i, a, lo, hi, out, depth)
  local cut = Divs.argmin(i, a, lo, hi)
  if   cut and depth > 0
  then Divs.split(i, a, lo,    cut, out, depth-1)
       Divs.split(i, a, cut+1, hi,  out, depth-1) 
  else out[ #out+1 ] = i.x(a[lo]) 
  end  
  return out
end 

function Divs.argmin(i,a,lo,hi,     out)
  local xl,xr,_, yl,yr,min = Divs.leftRight(i,a,lo,hi) 
  for arg = lo, hi do
    i.xis.add(xl, a[arg] ); i.xis.sub(xr, a[arg])
    i.yis.add(yl, a[arg] ); i.yis.sub(yr, a[arg])
    if arg > lo + i.step then
      if arg < hi - i.step then
        local now, after = i.x( a[arg] ), i.x( a[arg+1] )     
        if now ~= i.THE.skip then
          if now ~= after then 
            if after - i.start > i.epsilon then
              if i.stop  - now > i.epsilon then
                if i.xis.mid(xr) - i.xis.mid(xl) > i.epsilon then
                  local new = i.yis.xpect(yl,yr)
                  if new * i.THE.trivial < min then
                    min,out = new,arg 
  end end end end end end end end end 
  return out
end

function Divs.leftRight(i,a,lo,hi) 
  local xl, xr = i.xis.new{key=i.x}, i.xis.new{key=i.x}
  local yl, yr = i.yis.new{key=i.y}, i.yis.new{key=i.y}
  for j = lo,hi do i.xis.add(xr, a[j]); i.yis.add(yr, a[j]); end 
  i.epsilon = i.epsilon or i.xis.var(xr)*i.THE.cohen
  return  xl,xr,i.xis.var(xr),    yl,yr,i.yis.var(yr)
end

return Divs
