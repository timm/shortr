-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 
 
-- <img align=right src="https://github.com/timm/lua/raw/master/etc/img/divs.jpg">
-- 
-- This file returns a function that
-- divides a list of numbers such that the variability
-- of each division is minimized. 
--
-- To simplify that task, we sort the numbers once
-- then measure variance using the 10th and 90th percentile 
-- of that list. This has the advantage that, if we recurse
-- into some part of that list, then we can quickly estimate
-- the variance of that sublist using that list's 10th and 90th
-- percentile.

require "lib"
local THE=require("the").divs

local x,p,mid,var,xpect -- functions

function x(a,z)       return a[math.floor(z)] end
function p(a,z)       return x(a, z*#i.has ) end
function mid(a,lo,hi) return x(a, lo + .5*(hi-lo) ) end

-- On experimentation, we found dividing the 90th minus
-- 10th percentile by 2.7 returns a value close to the normal
-- Gaussian variance.

function var(a,lo,hi) 
  return (x(a,lo+.9*(hi-lo)) - x(a,lo+.1*(hi-lo)))/2.7 end

-- The expected value of two `Some`s is the weighed sum of
-- their variances.

function xpect(a,lo,j,hi)
  local n1, n2, n = j-lo+1, hi-j , hi - lo + 1
  return n1/n * var(a,lo,j) + n2/n * var(a,j,hi) end

-- When we seek divisions, 
-- ignore divisions that are too small (less than, say,
-- square root size of the list-- see the `step` var);
-- or that divide the numbers into bins of size less 
-- than `epsilon`
-- (less than 30% of the standard deviation).


return function(a)
  table.sort(a)
  local cuts    = {}
  local step    = math.floor((#a)^THE.step)
  local epsilon = var(a,1,#a)*THE.cohen
  local function div(lo,hi,     cut)
    local best = var(a,lo,hi)
    for j = lo+step, hi-step do
      local now, after = x(a, j), x(a, j+1)
      if now ~= after then 
        if after - a[1] > epsilon then
          if a[#a] - now > epsilon then
            if math.abs( mid(a,lo,j) - mid(a,j+1,hi) ) > epsilon then
              local new = xpect(a,lo,j,hi)
              -- Ignore new divisions that improve things
              -- by less than a `trivial` amount
              -- (say, 5%).
              if new * THE.trivial < best then
                 best,cut = new,j end end end end end  end
    return cut  
  end -- end div
  local function recurse(lo,hi,  cut)
    cut = div(lo,hi)
    if   cut 
    then recurse(lo, cut)
         recurse(cut+1, hi) 
    else cuts[ #cuts+1 ] = lo end  
  end -- end recurse
  recurse(1, #a)
  return cuts
end  
