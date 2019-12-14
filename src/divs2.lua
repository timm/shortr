-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 
 
-- Discretization of list of variables
-- Generates splits that reduce the expected value of
-- the variability after the split.
-- (and "variance" is measured in terms of standard deviation
-- and entropy for numeric and symbolic columns).
--
-- - When passed a simple list of numbers, this
-- discretizer divides according to that list's variability.
-- - When passed a list of tables, the splits of one column
-- are selected to minimize the variance a second column.

-- This discretizer is passed the functions `x,y` to
-- select for the first and  second column. If the
-- `y` function is not supplied, then `x` is used
-- for both columns.

-- `x` and `y` can select for numeric or symbolic values.
-- The optional `xis` and `yis` parameters control for
-- how the data is collected (and their usual values
-- are `Num` or `Sym`).

-- For example, to split the third numeric column to reduce the
-- variance in the fifth symbolic column then

--       Div.new( a, {x   = function(r) return r[3] end,
--                    y   = function(r) return r[5] end,
--                    xis = Num,
--                    yis = Sym})                       

local THE  = require("the")
local Lib  = require("lib")
local Object = require("object")
local Num  = require("num")
local Sym  = require("Sym")
local Divs = {is="Divs"}

o,r,copy,same,has = Lib.o, Lib.r, Lib.copy,Lib.same,Lib.has

-- The optional `t` parameter can be used to set
-- some options:` 

--      {x= first,       -- how to access column1
--       y= second,      -- how to access column2
--       xis= Num,       -- what to expect in column1
--       yis= Sym,       -- what to expect in column2
--       the={
--         most = 256,   -- use, at most, 256 items
--         skip = '?',   -- ignore cells with this value
--         step = 0.5,   -- min split size = items^step
--         depth= 1000,  -- how deep to recurse
--         trivial=1.05, -- ignore small reductions  
--         cohen=0.3}}   -- ignore splits under cohen*sd

function Divs.new(a, the) 
  local i = Object.new()
  i.me  = Divs
  i.the = has({xtype=Num, ytype=Num, fx=same, fy=same},
              copy(THE.divs))
  a     = Divs.some(i,a)
  i.the.start = i.fx( a[1] )
  i.the.stop  = i.fx( a[#a] )
  i.the.step  = math.floor(#a)^i.the.step
  return Divs.split(i, a, 1, #a, 
                    i.xtype.all(a,fx),
                    i.ytype.all(a,fy),
                    {}, 
                    t.depth or 1000)
end 

-- Ignoring the skipped values, sampling 
-- some random subset, sort the numbers using the `x` function.
function Divs.some(i,a)
  local out = {}
  for one in pairs(a) do
    if r() < i.the.most/#a then
      if i.the.fx(one) ~= i.the.skip then
        out[#out+1] = one end end end
  table.sort(out,
             function(y,z) return x(y) < y(z) end)
  return out
end

-- If we can find a cut point, recurse left and
-- right of the cut. If we are too deep, just stop
function Divs.split(i, a, lo, hi,x,y,out, depth)
  local cut,lx,ly, rx,ry = Divs.argmin(i, a, lo, hi)
  if   cut and depth > 0
  then Divs.split(i, a, lo,    cut, lx,ly, out, depth-1)
       Divs.split(i, a, cut+1, hi,  rx,ry, out, depth-1) 
  else out[ #out+1 ] = {x= i.xtype.all(a,fx,lo,hi),
                        y= i.ytype.all(a,fy,lo,hi)} end
  return out
end 

-- Here's a function that finds
-- the arg that minimizes the expected value
-- of the variability after the split.
-- As we walk from left to right across the list,
-- incrementally add the current `x,y` values
-- to the "left" lists `xl,yl`
-- while decrementing the "right" lists `xr,yr`.
function Divs.argmin(i,a,lo,hi,     out)
  local xl1,yl1,xr1,yr1
  local xl,xr,_, yl,yr,min = Divs.leftRight(i,a,lo,hi) 
  for arg = lo, hi do
    i.xtype.add(xl, a[arg] ); i.xtype.sub(xr, a[arg])
    i.ytype.add(yl, a[arg] ); i.ytype.sub(yr, a[arg])
    if arg > lo + i.step then
     if arg < hi - i.step then
      local now   = i.the.fx(a[arg  ])
      local after = i.the.fx(a[arg+1])     
      if now ~= i.the.skip then
       if now ~= after then 
        if after - i.start > i.epsilon then
         if i.stop  - now > i.epsilon then
          if i.xtype.mid(xr) - i.xtype.mid(xl) > i.epsilon then
           local new = i.yis.xpect(yl,yr)
           if new * i.THE.trivial < min then
             min,out, xl1,yl1, xr1, yr1 = new,arg, 
                                          copy(xl), copy(yl),
                                          copy(xr), copy(yr)
  end end end end end end end end end 
  return out,xl1,yl1, xr1, yr1
end

-- Here's a low level function that initialized
-- the "left" and "right" lists. The "left" lists
-- of `xl,yl` are initially empty while the "right"
-- lists initially contain everything between `lo` and `hi`.
function Divs.leftRight(i,a,lo,hi) 
  local xl = i.xis.new{key=i.x}
  local xr = i.xis.new{key=i.x}
  local yl = i.yis.new{key=i.y}
  local yr = i.yis.new{key=i.y}
  for j = lo,hi do 
    i.xis.add(xr, a[j])
    i.yis.add(yr, a[j])
  end 
  i.epsilon = i.epsilon or i.xis.var(xr)*i.THE.cohen
  return  xl,xr,i.xis.var(xr),
          yl,yr,i.yis.var(yr)
end

-- ----
-- And finally...
return Divs
