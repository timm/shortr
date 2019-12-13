-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

-- Simple, fast,  recursive discretizer which returns splits
-- that minimized the post-split expected value of the
-- standard deviation.

-- Standard deviation of a sub range is estimated using the 90th-10th percentile difference within
-- that sub-range (that exact calculation is (90th=10th)/2.56,
-- where 2.56 is a magic number derived from 
-- [this code](https://gist.github.com/timm/934d4664de105544e51cc67444aa8c60)).

local Lib=require("lib")
local r,abs,settings = Lib.r,Lib.abs, Lib.settings

return function(a, the)
  local x,p,mid,stdev,xpect,argmin -- local functions
  local out,a1 = {},{} -- local vars
  the = settings(the,{
             no     = "?",
             max    = 256,
             magic  = 2.56,
             f      = function(z) return z end,
             trivial= 1.05,
             cohen  = 0.3,
             epsilon = 0,
             step   = 0.5})
  -------------------------------------
  -- Support code.
  function x(z)      return a1[math.floor(z)] end
  function p(z)      return x(z*#a1 ) end
  function mid(i,j)  return x(i + .5*(j-i) ) end
  function stdev(i,j)  
    return abs((x(i+.9*(j-i)) - x(i+.1*(j-i)))/the.magic) end
  function xpect(i,m,j)
    local n=j-i+1
    return (m-i)/n*stdev(i,m) + (j-m -1)/n*stdev(m+1,j) end
  -- Main worker: don't cut if:
  -- 
  -- - you are within `epsilon` of  `start` or `stop`
  -- - the numbers before and after the break are the same
  -- - the break does not reduce the expected value of the
  --   standard deviation (by more than a `trivial` amount) 
  -- - the mean value of the two new breaks differ by
  --   less than `epsilon`
  function argmin(lo,hi,   min,new,cut)
    min = stdev(lo,hi)
    for j = lo + the.step, hi-the.step do
      local now, after = x(j), x(j+1)
      if now ~= after then 
       if after - the.start > the.epsilon then 
        if the.stop - now > the.epsilon  then
         if mid(j+1,hi) - mid(lo,j) > the.epsilon then
           local new = xpect(lo,j,hi) 
           if new * the.trivial < min then
             min,cut = new,j end end end end end end
    if   cut 
    then argmin(lo,   cut)
         argmin(cut+1, hi) 
    else out[ #out+1 ] = {a1[lo], mid(lo,hi) } end
    return out
  end 
  -------------------------------------
  -- Get ready.
  --
  -- - Only reason over a random
  -- selection of the numbers. 
  -- - Select numbers within the input list using the `f` function.
  -- - Sort all the 
  -- numbers only once.
  -- - Ignore _don't care_ symbols;
  -- - Divide the numbers into `steps` of size, say, square root of the total list.
  -- - Guess a value for a small `epsilon` using Cohen's rule.
  for _,one in pairs(a) do
    if the.f(one) ~= the.no then
      if r() < the.max/#a then 
        a1[#a1+1] = the.f(one) end end end
  table.sort(a1)
  the.step    = math.floor((#a1)^the.step)
  the.stop    = a1[#a1]
  the.start   = a1[1]
  the.epsilon = the.epsilon > 0 and the.epsilon 
                or stdev(1,#a1) * the.cohen

-- ------
-- Go!
-- 
-- Returns a list of pairs `{break, summary}` where the former
-- shows the lower bound of some division while the latter shows
-- a summary of the data in that division. For example `{{2,3},{5,7}}`
-- says that the data divides on 2 and 5 and in those two
-- regions, the mean values are 3 and 7 (respectively).

  return argmin(1, #a1)
end
