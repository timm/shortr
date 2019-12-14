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
  i = has({xtype=Num, ytype=Num, fx=same, fy=same},
          copy(THE.divs))
  a     = Divs.some(i,a)
  i.start = i.fx( a[1] )
  i.stop  = i.fx( a[#a] )
  i.step  = math.floor(#a)^i.the.step
  return Divs.split(i, a, 1, #a, 
                    i.xtype.all(a,fx),
                    i.ytype.all(a,fy),
                    {}, 
                    t.depth or 1000)
end 

-- Ignoring the skipped values, sampling 
-- some random subset, sort the numbers using the `x` function.
function Divs.some(i,a,    out)
  out = {}
  for one in pairs(a) do
    if r() < i.most/#a then
      if i.fx(one) ~= i.skip then
        out[#out+1] = one end end end
  table.sort(out, function(y,z) 
                    return i.fx(y) < i.fy(z) end)
  return out
end

-- If we can find a cut point, recurse left and
-- right of the cut. If we are too deep, just stop
function Divs.split(i, a, lo, hi,x,y,out, depth)
  local cut,lx,ly, rx,ry = Divs.argmin(i, a, lo, hi)
  if   cut and depth > 0
  then Divs.split(i, a, lo,    cut, lx,ly, out, depth-1)
       Divs.split(i, a, cut+1, hi,  rx,ry, out, depth-1) 
  else 
       out[ #out+1 ] = {x= i.xtype.all(a,i.fx,lo,hi),
                        y= i.ytype.all(a,i.fy,lo,hi)} end
  return out
end 

-- Here's a function that finds
-- the arg that minimizes the expected value
-- of the variability after the split.
-- As we walk from left to right across the list,
-- incrementally add the current `x,y` values
-- to the "left" lists `xl,yl`
-- while decrementing the "right" lists `xr,yr`.
function Divs.argmin(i,a,lo,hi,xr,yr,     out)
  local xl1,yl1,  xr1,yr1, min, xl, yl
  i.epsilon = i.epsilon or i.xtype.var(xr)*i.cohen
  min       = i.ytype.var(yr)
  xl        = i.xtype.new{key=i.fx}
  yl        = i.ytype.new{key=i.fy}
  for j = lo, hi do
    i.xtype.add(xl, a[j])
    i.ytype.add(yl, a[j])
    i.xtype.sub(xr, a[j])
    i.ytype.sub(yr, a[j])
    if j > lo + i.step and
       j < hi - i.step 
    then
      local now   = i.fx(a[j  ])
      local after = i.fx(a[j+1])     
      if now ~= i.skip and
        now  ~= after  and 
        after  - i.start > i.epsilon and
        i.stop - now     > i.epsilon and
        i.xtype.mid(xr) - i.xtype.mid(xl) > i.epsilon 
      then
        local new = i.ytype.xpect(yl,yr)
        if new * i.trivial < min 
        then
          min, out = new, j
          xl1,yl1,xr1,yr1 = copy(xl),copy(yl),copy(xr),copy(yr) 
          end end end end 
  return out,xl1,yl1, xr1, yr1
end

-- ----
-- And finally...
return Divs
