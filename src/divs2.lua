-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

-- <img  align=right width=250
--   src= "http://raw.githubusercontent.com/timm/lua/master/etc/img/divs2.png">
-- ## Synopsis
-- Supervised descretization.
-- ## Descriptions
-- Given a list of list, split one column (called `x`)
-- into order to most reduce
-- the expected value of
-- the another column (called `y`).
-- Here,  "variance" refers to the
-- standard deviation
-- and entropy for numeric and symbolic columns).
--
-- - When passed a simple list of numbers, this
-- discretizer divides according to that list's variability.
-- - When passed a list of tables, the splits of one column
-- are selected to minimize the variance a second column.
-- ## Options
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

-- Another interesting option is `depth`. To get only one
-- split (with no subsequent recursion), use `{depth=1}`
-- This is useful during  learning trees when you want
-- to give subtrees as much data as possible.
-- ## Code
local THE  = require("the")
local Lib  = require("lib")
local Num  = require("num")
local Sym  = require("sym")

local r,copy,same,has = Lib.r, Lib.copy,Lib.same,Lib.has

local function all(a, my) 
  local out = {}
  my = has(my)(THE.divs)
  my = has(my){xtype=Num, ytype=Num, fx=same, fy=same}
   -- Look for some split between `lo` and `hi`
  -- that minimizes the `var` property of the `fy` variables.
  -- looking for the Walk across our list from lo to hi,
  -- incrementally add the current `x,y` values
  -- to the "left" lists `xl,yl`
  -- while decrementing the "right" lists `xr,yr`.
  local function argmin(lo,hi,lvl, xall, yall)
    local xl1,yl1,xr1,yr1,out, now,after, new
    local min = my.ytype.var(yall)
    local yvar = min
    local ymid = my.ytype.mid(yall)
    local xr  = xall
    local yr  = yall
    local xl  = my.xtype.new{key=i.fx}
    local yl  = my.ytype.new{key=i.fy}
    if lvl < my.depth then
      for j = lo, hi do
        my.xtype.add(xl, a[j]); my.ytype.add(yl, a[j])
        my.xtype.sub(xr, a[j]); my.ytype.sub(yr, a[j])
        if j > lo + my.step and j < hi - my.step 
        then
          now   = my.fx(a[j  ])
          after = my.fx(a[j+1])     
          if now  ~= after                and 
             after  - my.start > my.epsilon and
             my.stop - now     > my.epsilon and
             my.xtype.mid(xr) - my.xtype.mid(xl) > i.epsilon 
          then
            new = my.ytype.xpect(yl,yr)
            if new * my.trivial < min 
            then
              min, out = new, j
              xl1,yl1,xr1,yr1 = copy(xl),copy(yl),copy(xr),copy(yr) 
              end end end end 
    if cut then
        -- If we can find somewhere to split, then recurse.
      argmin(lo,    cut, lvl+1, lx,ly)
      argmin(cut+1, hi,  lvl+1, rx,ry) 
    else
      -- If no new split, then add all of `lo` to `hi`
      -- into the `out` list. Create a record `{x=..,yvar=..}`
      -- showing a summary of the `x ` and `y` values in this region.
      out[#out+1] = {fx=fx. lo=b4, n= hi-lo+1, hi=my.fx(h1), 
                     var=yvar, mid=ymid}
      b4 = my.fx(hi)
  end 
  
  -- Main function. Set up lots of locals, the start
  -- `recurse`ing to find the splits.
  my.start   = my.fx( a[1] )
  my.stop    = my.fx( a[#a] )
  my.step    = math.floor(#a)^my.step
  local yall = my.ytype.all(a,my.fy)
  local xall = my.xtype.all(a,my.fx)
  if my.epsilon == 0 then
    my.epsilon = i.xtype.var(xall)*i.cohen
  end
  out = {}
  b4  = math.mininteger
  argmin(1, #a,1, xall, yall)
  out[#out].hi = math.maxinteger
  return out
end

local function some(a,my)
  my = has(my){THE.divs}
  local a1={}
  for _,one in pairs(a) do
    if my.fx(one) ~= my.skip then
      if r() < my.most/#a then 
        a1[#a1+1] = one end end 
  end
  local function order(z1,z2) return my.fx(z1) < my.fx(z2) end
  table.sort(a1, order)
  return all(a1,my)
end

return {all=all,some=some}


