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

o,r,copy,same = Lib.o, Lib.r, Lib.copy,Lib.same

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

function Divs.new(a, t) 
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

-- Ignoring the skipped values, sampling 
-- some random subset, sort the numbers using the `x` function.
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


-- If we can find a cut point, recurse left and
-- right of the cut. If we are too deep, just stop
function Divs.split(i, a, lo, hi, out, depth)
  local cut = Divs.argmin(i, a, lo, hi)
  if   cut and depth > 0
  then Divs.split(i, a, lo,    cut, out, depth-1)
       Divs.split(i, a, cut+1, hi,  out, depth-1) 
  else out[ #out+1 ] = i.x(a[lo]) 
  end  
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
