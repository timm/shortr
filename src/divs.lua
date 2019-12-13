-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

local Lib=require("lib")
local r, o, round, abs = Lib.r, Lib.o, Lib.round, Lib.abs

local function pre(n) return string.rep("|.. ",n) end
local function s(n) return round(n,3)  end

return  function (a,t)
  local no,max,f,step,stop,start, epsilon, trivial,cohen
  local out,a1, step, stop, start -- local variables
  local x,p,mid,var,expect,argmin,recurse -- local functions
  -------------------------------------
  -- Support
  function x(z)      return a1[math.floor(z)] end
  function p(z)      return x(z*#a1 ) end
  function mid(i,j) return x(i + .5*(j-i) ) end
  function var(i,j)  return abs((x(i+.9*(j-i)) - x(i+.1*(j-i)))/2.7) end
  function xpect(i,m,j)
    local n=j-i+1
    return (m-i)/n*var(i,m) + (j-m -1)/n*var(m+1,j) end
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
  out ={}
  argmin(1, #a1,1)
  return out
end
