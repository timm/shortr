-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

-- <img  align=right 
--   src= "http://raw.githubusercontent.com/timm/lua/master/etc/img/divs.jpg">
-- ## Synopsis
-- Simple, fast,  recursive discretizer.
--
-- ## Description
-- Splits a list of numbers such that the
-- the expected value of the standard deviation (after the splits) is minimized.

-- Standard deviation of a split is estimated using the 
-- 90th-10th percentile difference within
-- that sub-range. That exact calculation is (90th=10th)/2.56,
-- where 2.56 is a magic number derived from 
-- [this code](https://gist.github.com/timm/934d4664de105544e51cc67444aa8c60).

-- ## Return Value
-- 
-- Returns a list of `split`s of the form
-- {lo=lo, hi=hi, etc}
-- where a numbers `x`  is in a range if 
-- `lo <= x <= hi`. This code sets the `lo`
-- of the first range to `math.mininteger`
-- and the `hi` of the last range to `math.maxinteger`.

local THE=require("the")
local Lib=require("lib")
local r,abs,has = Lib.r,Lib.abs, Lib.has

-- -----------------------------------
-- ## all
-- Divide all the data.
-- Use all the data in `a`, return `ranges`.
-- Assumes `a` is sorted.
local  function all(a, my)
  local x,p,mid,stdev,xpect,argmin,splits-- local functions
  my = has(my)(THE.divs)
    -- A good break does *not* 
  -- (a) fall `epsilon` of  `start` or `stop`;
  -- (b) have the same number before and after each break;
  -- (c) reduce the expected value of the variability by less than
  --     a `trivial` mount.
  -- (d) divide the ranges by more than `depth` levels.
  function argmin(lo,hi,lvl,       cut)
    local here={fx = fx,  
                lo = splits[#splits] and splits[#splits].hi 
                     or math.mininteger, 
                hi = x(hi), 
                n  = hi-lo+1, 
                mid= mid(lo,hi),
                var= stdev(lo,hi)}
    if lvl < my.depth then
      if hi - lo > my.step then
        local min  = here.var
        for j = lo + my.step, hi-my.step do
          local now, after = x(j), x(j+1)
          if now ~= after                  and 
             after - my.start > my.epsilon and 
             my.stop - now    > my.epsilon and
             mid(j+1,hi) - mid(lo,j) > my.epsilon 
          then
             local new = xpect(lo,j,hi) 
             if new * my.trivial < min then
               min,cut = new,j end end end end
    end -- end if 
    if cut then
      argmin(lo,    cut, lvl+1)
      argmin(cut+1, hi,  lvl+1)
    else
      splits[#splits+1] = here 
    end
  end 
  -- Some details
  function x(z)      return my.fx(a[math.floor(z)]) end
  function p(z)      return x(z*#a ) end
  function mid(i,j)  return x(i + .5*(j-i) ) end
  function stdev(i,j)  
    return abs((x(i+.9*(j-i)) - x(i+.1*(j-i)))/my.magic) end
  function xpect(i,m,j)
    local n=j-i+1
    return (m-i)/n*stdev(i,m) + (j-m -1)/n*stdev(m+1,j) end

  my.start= x(1)  -- smallest value
  my.stop = x(#a) -- largest value
  my.step = math.floor((#a)^my.step)-- need at least "step" items
  -- Guess a value for a small `epsilon` using Cohen's rule.
  if my.epsilon == 0 then 
    my.epsilon = stdev(1,#a) * my.cohen 
  end
  -- Go to work
  splits = {}
  argmin(1, #a,1)
  splits[1].lo    = math.mininteger
  splits[#splits].hi = math.maxinteger
  return splits
end

-- ## some
-- - Only reason over a random -- selection of my numbers. 
-- - Select numbers within my input list using my `f` function.
-- - Sort all my 
-- numbers only once.
-- - Ignore _don't care_ symbols;

-- Using a random sample of data from `a`,
-- and ignoring all the entries with `fx="?"`,
-- first sort the sample,  then return the `ranges`. 
local function some(a,my)
  my = has(my)(THE.divs)
  local a1={}
  for _,one in pairs(a) do
    if my.fx(one) ~= my.skip then
      if r() < my.most/#a then 
        a1[#a1+1] = one end end 
  end
  local function order(z1,z2) 
          return my.fx(z1) < my.fx(z2) end
  table.sort(a1, order)
  return all(a1,my)
end

return {all=all,some=some}

-- ## Author

-- Written by  Tim Menzies.
