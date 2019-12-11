-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 
 
-- This file returns a function that
-- divides a list of numbers such that the variability
-- of each division is minimized. 
--

local THE  = require("the")
local r    = require("lib").r
local Num  = require("num")
local Sym  = require("Sym")
local Divs = {is="Divs"}

function Divs.new(a, t) -- col,depth,x,xis,y,ys
  local i = Object.new(t)
  i.me    = Divs
  t       = t or {}
  a       = Divs.sort(i,a, t.most or THE.divs.most)
  i.xis   = t.xis or Num
  i.x     = t.x   or function(r) row.cells[col] end
  i.yis   = t.yis or i.xis
  i.y     = t.y   or i.x
  i.start,i.stop = i.x(a[1]), i.x(a[#a])
  i.step  =  math.floor(#a)^THE.divs.step
  return Divs.recurse(i, a, 1, #a, {}, t.depth or 1)
end 

function Divs.sort(i,a,most,    b)
  for _,one in pairs(a) do
    if r() < most/#a then
      if i.x(one) ~= "?" then
        b[#b+1] = one end end end
  table.sort(b, 
         function(u,v) return i.x(u) < i.x(v) end)
  return b
end

function Divs.recurse(i, a, lo, hi, out, depth)
  local cut = Divs.argmin(i, a, lo, hi)
  if   cut and depth > 0
  then Divs.recurse(i, a, lo,    cut, out, depth-1)
       Divs.recurse(i, a, cut+1, hi,  out, depht-1) 
  else out[ #out+1 ] = i.x(a[lo]) 
  end  
  return out
end 

function Divs.argmin(i,a,lo,hi,     cut)
  local xl,xr,_, yl,yr,best = Divs.leftRight(i,a,lo,hi) 
  for j = lo, hi do
    xis.add(xl, a[j] ); xis.sub(xr, a[j])
    yis.add(yl, a[j] ); yis.sub(yr, a[j])
    if j > lo + step then
      if j < hi - step then
        local now, after = i.x( a[j] ), i.x( a[j+1] )     
        if now ~= after then 
          if after - start > epsilon then
            if stop  - now > epsilon then
              if xis.mid(xr)  - xis.mid(xl) > epsilon then
                local new = yis.xpect(yl,yr)
                if new * THE.divs.trivial < best then
                  best,cut = new,j 
  end end end end end end end end 
  return cut
end

function Divs.leftRight(i,a,lo,hi, 
                        xl,xr,xsd, yl,yr,ysd)
  xl,xr = i.xis.new{key=i.x}, i.xis.new{key=i.x}
  yl,yr = i.yis.new{key=i.y}, i.yis.new{key=i.y}
  for j = lo,hi do
    i.xis.add(xr, a[j])
    i.yis.add(yr, a[j]) end 
  xsd = i.xis.var(xr)
  ysd = i.yis.var(yr)
  i.epsilon = i.epsilon or xsd*THE.divs.cohen
  return  xl,xr,xsd,   yl,yr,ysd
end
