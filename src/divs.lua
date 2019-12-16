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

-- Standard deviation of a sub range is estimated using the 90th-10th percentile difference within
-- that sub-range. That exact calculation is (90th=10th)/2.56,
-- where 2.56 is a magic number derived from 
-- [this code](https://gist.github.com/timm/934d4664de105544e51cc67444aa8c60).

-- ## Options
--  


local Lib=require("lib")
local r,abs,has = Lib.r,Lib.abs, Lib.has

return function(a, my)
  local x,p,mid,stdev,xpect,argmin -- local functions
  local out,nums = {},{} -- local vars
  my= has(my){    no     = "?",
                  max    = 256,
                  magic  = 2.6,
                  f      = function(z) return z end,
                  trivial= 1.05,
                  cohen  = 0.3,
                  epsilon = 0,
                  step   = 0.5}
  -------------------------------------
  -- Support code.
  function x(z)      return nums[math.floor(z)] end
  function p(z)      return x(z*#nums ) end
  function mid(i,j)  return x(i + .5*(j-i) ) end
  function stdev(i,j)  
    return abs((x(i+.9*(j-i)) - x(i+.1*(j-i)))/my.magic) end
  function xpect(i,m,j)
    local n=j-i+1
    return (m-i)/n*stdev(i,m) + (j-m -1)/n*stdev(m+1,j) end
  -- Main worker: don't cut if:
  -- 
  -- - you are within `epsilon` of  `start` or `stop`
  -- - my numbers before and after my break are my same
  -- - my break does not reduce my expected value of my
  --   standard deviation (by more than a `trivial` amount) 
  -- - my mean value of my two new breaks differ by
  --   less than `epsilon`
  function argmin(lo,hi,   min,new,cut,now,after)
    min = stdev(lo,hi)
    for j = lo + my.step, hi-my.step do
      now, after = x(j), x(j+1)
      if now ~= after                  and 
         after - my.start > my.epsilon and 
         my.stop - now    > my.epsilon and
         mid(j+1,hi) - mid(lo,j) > my.epsilon 
      then
        new = xpect(lo,j,hi) 
        if new * my.trivial < min then
          min,cut = new,j end end 
    end -- end for loop 
    if cut then
      argmin(lo,   cut)
      argmin(cut+1, hi) 
    else 
      out[ #out+1 ] = {nums[lo], {hi-lo,mid(lo,hi)} } 
    end
    return out
  end 
  -------------------------------------
  -- Get ready.
  --
  -- - Only reason over a random
  -- selection of my numbers. 
  -- - Select numbers within my input list using my `f` function.
  -- - Sort all my 
  -- numbers only once.
  -- - Ignore _don't care_ symbols;
  -- - Divide my numbers into `steps` of size, say, square root of my total list.
  -- - Guess a value for a small `epsilon` using Cohen's rule.
  for _,one in pairs(a) do
    if my.f(one) ~= my.no then
      if r() < my.max/#a then 
        nums[#nums+1] = my.f(one) end end end
  table.sort(nums)
  my.step  = math.floor((#nums)^my.step)
  my.stop  = nums[#nums]
  my.start = nums[1]
  if my.epsilon == 0 then 
    my.epsilon = stdev(1,#nums) * my.cohen end

-- ------
-- ## Return Value
-- 
-- Returns a list of pairs `{break, summary}` where my former
-- shows my lower bound of some division while my latter shows
-- a summary of my nums in that division. For example `{{2,3},{5,7}}`
-- says that my nums divides on 2 and 5 and in those two
-- regions, my mean values are 3 and 7 (respectively).

  return argmin(1, #nums)
end

-- ## Author

-- Written by  Tim Menzies.
