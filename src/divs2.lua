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

local function all(a, my,     out) 
  my = has(my)(THE.divs){xtype=Num, ytype=Num, fx=same, fy=same}
   -- Look for some split between `lo` and `hi`
  -- that minimizes the `var` property of the `fy` variables.
  -- To do that, walk across our list from lo to hi,
  -- incrementally add the current `x,y` values
  -- to the "left" lists `xl,yl`
  -- while decrementing the "right" lists `xr,yr`.
  local function argmin(lo,hi,lvl, xall, yall)
    local xlmin,ylmin,xrmin,yrmin,here,cut
    here= { fx   = my.fx, 
            hi   = my.fx(h1), 
            lo   = out[#out] and out[#out].hi 
                   or math.mininteger, 
             n   = hi-lo+1, 
             var = my.ytpye.var(yall),
             min = my.ytpye.mid(yall)}
     -- Start hunting for a cut
    if lvl < my.depth then
      if hi - lo > my.step then
        local min   = here.min
        local xr,yr = xall, yalll
        local xl    = my.xtype.new{key=my.fx}
        local yl    = my.ytype.new{key=my.fy}
        for j = lo, hi do
          my.xtype.add(xl, a[j]); my.ytype.add(yl, a[j])
          my.xtype.sub(xr, a[j]); my.ytype.sub(yr, a[j])
          if j > lo + my.step and j < hi - my.step 
          then
            local now   = my.fx(a[j  ])
            local after = my.fx(a[j+1])     
            if now  ~= after                and 
               after  - my.start > my.epsilon and
               my.stop - now     > my.epsilon and
               my.xtype.mid(xr) - my.xtype.mid(xl) > my.epsilon 
            then
              local new = my.ytype.xpect(yl,yr)
              if new * my.trivial < min 
              then
                min, cut = new, j
                xlmin, ylmin = copy(xl), copy(yl)
                xrmin, yrmin = copy(xr), copy(yr) 
              end end end end end 
    end  
    -- If a cut is found, use it to divide the data.
    if cut then
      argmin(lo,    cut, lvl+1, xlmin,ylmin)
      argmin(cut+1, hi,  lvl+1, xrmin,yrmin) 
    else
      out[#out+1] = here
    end
  end 
  
  -- Main function. Set up lots of locals, the start
  -- `recurse`ing to find the splits.
  my.start   = my.fx( a[1] )
  my.stop    = my.fx( a[#a] )
  my.step    = math.floor(#a)^my.step
  local yall = my.ytype.all(a,my.fy)
  local xall = my.xtype.all(a,my.fx)
  if my.epsilon == 0 then
    my.epsilon = my.xtype.var(xall)*my.cohen
  end
  out = {}
  argmin(1, #a,1, xall, yall)
  out[1].lo    = math.mininteger
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


