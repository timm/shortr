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

-- This discretizer is passed the functions `fx,fy` to
-- select for the first and  second column. 

-- `fx` and `fy` can select for numeric or symbolic values.
-- The optional `xtype` and `ytype` parameters control for
-- how the data is collected (and their usual values
-- are `Num` or `Sym`).

-- For example, to split the third numeric column to reduce the
-- variance in the fifth symbolic column then

--      divs( a, {fx   = function(r) return r[3] end,
--                fy   = function(r) return r[5] end,
--                xtype = Num,
--                ytype = Sym})                       

local THE  = require("the")
local Lib  = require("lib")
local Num  = require("num")
local Sym  = require("Sym")

local r,copy,same,has = Lib.r, Lib.copy,Lib.same,Lib.has

local function some(i,a,      out)
  out = {}
  for _,one in pairs(a) do
    if i.fx(one) ~= i.skip and r() < i.most/#a then 
      out[#out+1] = one end end
  table.sort(out, function(y,z) 
                    return i.fx(y) < i.fx(z) end)
  return out
end

-- Look for some split between `lo` and `hi`
-- that minimizes the `var` property of the `fy` variables.
-- looking for the Walk across our list from lo to hi,
-- incrementally add the current `x,y` values
-- to the "left" lists `xl,yl`
-- while decrementing the "right" lists `xr,yr`.
local function argmin(i,a,lo,hi,xall, yall,     out)
  local xl1,yl1,xr1,yr1,out, now,after, new
  local min = i.ytype.var(yall)
  local xr  = xall
  local yr  = yall
  local xl  = i.xtype.new{key=i.fx}
  local yl  = i.ytype.new{key=i.fy}
  for j = lo, hi do
    i.xtype.add(xl, a[j]); i.ytype.add(yl, a[j])
    i.xtype.sub(xr, a[j]); i.ytype.sub(yr, a[j])
    if j > lo + i.step and j < hi - i.step 
    then
      now   = i.fx(a[j  ])
      after = i.fx(a[j+1])     
      if now  ~= after                and 
         after  - i.start > i.epsilon and
         i.stop - now     > i.epsilon and
         i.xtype.mid(xr) - i.xtype.mid(xl) > i.epsilon 
      then
        new = i.ytype.xpect(yl,yr)
        if new * i.trivial < min 
        then
          min, out = new, j
          xl1,yl1,xr1,yr1 = copy(xl),copy(yl),copy(xr),copy(yr) 
          end end end end 
  return out,xl1,yl1, xr1, yr1
end

-- As we recurse, take the stats the upper level
-- and split those into `x,y` values, left and right 
-- of the split (into the lsts `xl,yl, xr,yr`).
local function recurse(i,a,lo,hi,xall,yall,out,depth,x0,yvar)
  x0   = copy(xall) 
  yvar = i.ytype.var(yall)
  local cut,lx,ly, rx,ry = argmin(i,a,lo, hi,xall,yall)
  if   cut and depth > 0
  then 
       -- If we can find somewhere to split, then recurse.
       recurse(i,a,lo,    cut, lx,ly, out, depth-1)
       recurse(i,a,cut+1, hi,  rx,ry, out, depth-1) 
  else 
       -- If no new split, then add all of `lo` to `hi`
       -- into the `out` list. Create a record `{x=..,yvar=..}`
       -- showing a summary of the `x ` and `y` values in this region.
       x0.lo = i.fx(a[lo])
       x0.hi = i.fx(a[hi])
       out[ #out+1 ] = {x=x0, yvar=yvar} end
  return out
end 

-- Main function. Set up lots of locals, the start
-- `recurse`ing to find the splits.
return function (a, i) 
  i = has(i)(THE.divs)
  i = has(i){xtype=Num, ytype=Num, fx=same, fy=same}
  a         = some(i,a)
  i.start   = i.fx( a[1] )
  i.stop    = i.fx( a[#a] )
  i.step    = math.floor(#a)^i.step
  local y   = i.ytype.all(a,i.fy)
  local x   = i.xtype.all(a,i.fx)
  i.epsilon = i.epsilon or i.xtype.var(x)*i.cohen
  return recurse(i,a, 1, #a, x, y, {}, i.depth or 1000)
end 
