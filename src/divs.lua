-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

-- Simple, fast,  recursive discretizer. Sorts all the 
-- numbers once, then estimates standard deviation for any
-- sub range using the 90th-10th percentile difference within
-- that sub-range.

local Lib=require("lib")
local r, o, round, abs = Lib.r, Lib.o, Lib.round, Lib.abs

-- -------
-- Magic parameter. 
-- Set using [this code](https://gist.github.com/timm/934d4664de105544e51cc67444aa8c60).
-- Standard deviation of a sub-range estiamted via `(90th-10th)/magic`.

local magic=2.56

-- --------------
-- The worker
local   function divs (a,t)
  local no,max,f,step,stop,start, epsilon, trivial,cohen -- config
  local out,a1, step, stop, start -- local vars
  local x,p,mid,var,expect,argmin,recurse -- local functions
  -------------------------------------
  -- Support
  function x(z)      return a1[math.floor(z)] end
  function p(z)      return x(z*#a1 ) end
  function mid(i,j)  return x(i + .5*(j-i) ) end
  function var(i,j)  
    return abs((x(i+.9*(j-i)) - x(i+.1*(j-i)))/magic) end
  function xpect(i,m,j)
    local n=j-i+1
    return (m-i)/n*var(i,m) + (j-m -1)/n*var(m+1,j) end
  -------------------------------------
  -- Main worker
  function argmin(lo,hi, depth ,  min,new,cut)
    min = var(lo,hi)
    for j = lo+step, hi-step do
      local now, after = x(j), x(j+1)
      if now ~= after then 
       if after - start > epsilon then 
        if stop - now > epsilon  then
         if mid(j+1,hi) - mid(lo,j) > epsilon then
           local new = xpect(lo,j,hi) 
           if new * trivial < min then
             min,cut = new,j end end end end end end
    if   cut 
    then argmin(lo,   cut,depth+1)
         argmin(cut+1, hi,depth+1) 
    else out[ #out+1 ] = a1[lo] 
    end  
  end 
  -------------------------------------
  -- Config
  t       = t or {}
  no      = t.no or "?"
  max     = t.max or 256
  f       = t.f or function(z) return z end
  trivial = t.trivial or 1.05
  cohen   = t.cohen   or 0.30
  a1      = {}
  for _,one in pairs(a) do
    if f(one) ~= no then
      if r() < max/#a then a1[#a1+1] = f(one) end end end
  table.sort(a1, function(y,z) return f(y) < f(z) end)
  step    = math.floor(t.step or (#a1)^0.5)
  stop    = a1[#a1]
  start   = a1[1]
  epsilon = t.epsilon or var(1,#a1) * cohen
  -------------------------------------
  -- Let's go!
  out ={}
  argmin(1, #a1,1)
  return out
end

return divs
