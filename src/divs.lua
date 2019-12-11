-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 
 
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

local THE=require("the").divs

local x,p,mid,var,xpect -- functions

local function x(a,z)       return a[math.floor(z)] end
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

local  function div(a, col,  t, -- maxDepth,x,xis, y, yis,  
                    epsilon, start,stop, step, out)
  out = {}
  no  = THE.char.skip
  a   = Some.all( a, Some.new{most=t.most}).has
  maxDepth= t.maxDepth or 1
  xis = t.xis or Num
  x   = t.x   or  function(r) row.cells[col] end
  yis = t.yis or xis
  y   = t.y   or x
  table.sort(a, function(u,v) 
                  return x(u)==no or x(v)==no or x(u) < x(v) end)
  start,stop = x(a[1]), x(a[#a])
  step       =  math.floor(#a)^THE.divs.step
  local function leftRight(lo,hi)
    local xl,xr= xis.new{key=x}, xis.new{key=x}
    local yl,yr= yis.new{key=y}, yis.new{key=y}
    for j = lo,hi do
      xis.add(xr, a[j])
      yis.add(yr, a[j])
    end
    local xsd = xis.var(xr)
    local ysd = yis.var(yr)
    epsilon  = epsilon or xsd*THE.divs.cohen
    return xl,xr,xsd,   yl,yr,ysd
  end
  local function div(lo,hi,     cut)
    local xl,xr,xbest, yl,yr,ybest = leftRight(lo,hi) 
    for j = lo, hi do
      xis.add(xl, a[j] ); xis.sub(xr, a[j])
      yis.add(yl, a[j] ); yis.sub(yr, a[j])
      if j > lo + step then
        if j < hi - step then
          local now, after = x(a[j]), x(a[j+1])     
          if now ~= after then 
            if after - start > epsilon then
              if stop  - now > epsilon then
                if xis.mid(xr)  - xis.mid(xl) > epsilon then
                  local new = yis.xpect(yl,yr)
                  -- Ignore new divisions that improve things
                  -- by less than a `trivial` amount
                  -- (say, 5%).
                  if new * THE.divs.trivial < ybest then
                    ybest,cut = new,j end end end end end end end end 
    maxDepth = maxDepth - 1
    if   cut and maxDepth > 0
    then div(lo, cut)
         div(cut+1, hi) 
    else cuts[ #cuts+1 ] = x(a[lo]) end  
  end -- end recurse
  div(1, #a)
  return cuts
end  
