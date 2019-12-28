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
local map,o = Lib.map, Lib.o

local function all(a, my,     splits) 
  my = has(my)(THE.divs){xtype=Num, ytype=Num, fx=same, fy=same}
   -- A good break does *not* 
  -- (a) fall `epsilon` of  `start` or `stop`;
  -- (b) have the same number before and after each break;
  -- (c) reduce the expected value of the 
  --     y-value variability by less than
  --     a `trivial` mount.
  -- (d) divide the splits by more than `depth` levels.
  
  -- To do that, walk across our list from lo to hi,
  -- incrementally add the current `x,y` values
  -- to the "left" lists `xl,yl`
  -- while decrementing the "right" lists `xr,yr`.
  local function argmin(lo,hi,lvl, xall, yall)
    local xlmin,ylmin,xrmin,yrmin,cut
        -- Start hunting for a cut
    local yall1=copy(yall)
    if lvl < my.depth then
      if hi - lo > my.step then
        local min   = my.ytype.var(yall)
        local xr,yr = xall, yall
        local xl    = my.xtype.new{key=my.fx}
        local yl    = my.ytype.new{key=my.fy}
        for j = lo, hi do
          my.xtype.add(xl, a[j]); my.ytype.add(yl, a[j])
          my.xtype.sub(xr, a[j]); my.ytype.sub(yr, a[j])
          if j > lo + my.step and j < hi - my.step 
          then
            local after = my.fx(a[j+1])     
            local now   = my.fx(a[j  ])
            if now ~= THE.char.skip         and 
               now  ~= after                and 
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
      local here= {fx= my.fx, 
                   hi= my.fx(a[hi]), 
                   lo= splits[#splits] and splits[#splits].hi 
                       or math.mininteger,
                   _all={},
                   stats=yall1}
      here.use = function(r,  x) x = here.fx(r)
                     return here.lo < x and x <= here.hi end
      here.show= string.format("(%s..%s]", here.lo, here.hi)
      for j=lo,hi do here._all[#(here._all)+1] = row end
      splits[#splits+1] = here
    end
  end 
  
  -- Main function. Set up lots of locals, the start
  -- `recurse`ing to find the splits.
  my.start   = my.fx( a[1] )  -- smallest value
  my.stop    = my.fx( a[#a] ) -- largest value
  my.step    = math.floor(#a)^my.step-- need at least "step" items
  -- Guess a value for a small `epsilon` using Cohen's rule.
  local yall = my.ytype.all(a,my.fy)
  local xall = my.xtype.all(a,my.fx)
  if my.epsilon == 0 then
    my.epsilon = my.xtype.var(xall)*my.cohen
  end
  splits = {}
  argmin(1, #a,1, xall, yall)
  splits[1].lo       = math.mininteger
  splits[#splits].hi = math.maxinteger
  return splits
end

-- ------
-- ## Complete
-- If the splits are computed via `some` then only some
-- the entries in `a` will be used to find splits. If so,
-- then we need a little post-processor to `some` to `complete`
-- in the stats using all the y values from `a`.

local function complete(a, my, splits)
  for _,r in pairs(splits) do
    r.stats = my.ytype.new{key=my.fy}
    r._all ={}
  end
  for _,one in pairs(a) do
    for _,r in pairs(splits) do
      if r.use(one) then
        r._all[#(r._all) + 1] = one
        my.ytype.add(r.stats, one) 
        break
        end end end
  return splits
end

-- --------
-- ## Some
-- Run the discretizer using some subset of the data.
-- - Only reason over a random -- selection of my numbers. 
-- - Select numbers within my input list using my `f` function.
-- - Sort all my 
-- numbers only once.
-- - Ignore _don't care_ symbols;

-- Using a random sample of data from `a`,
-- and ignoring all the entries with `fx="?"`,
-- first sort the sample,  then return the `splits`. 
local function some(a,my)
  my = has(my)(THE.divs){xtype=Num, ytype=Num, fx=same, fy=same}
  local a1={}
  for _,one in pairs(a) do
    if my.fx(one) ~= my.skip then
      if r() < my.most/#a then 
        a1[#a1+1] = one end end 
  end
  local function order(z1,z2) return my.fx(z1) < my.fx(z2) end
  table.sort(a1, order)
  return complete(a, my, all(a1,my))
end

return {all=all,some=some}

-- ## Author

-- Written by  Tim Menzies.
