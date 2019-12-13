-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

local r=require("lib").r

return function (a,t)
  local no,max,f,step,stop,start, epsilon, trivial,cohen
  local out,a1, step, stop, start -- local variables
  local x,p,mid,var,expect,argmin,recurse -- local functions
  -- support
  function x(z)      return a1[math.floor(z)] end
  function p(z)      return x(z*#a1 ) end
  function mid(i,jk) return x(i + .5*(j-i) ) end
  function var(i,j)  return (x(i+.9*(j-i)) - x(i+.1*(j-i))) end
  function xpect(i,m,j)
    return (m-j+1)/(j-i)*var(i,m) + (hi-m)/(j-i)*var(j+1,hi) end
  -- config
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
  step    = t.step or (#a1)^0.5
  stop    = a1[#a1]
  start   = a1[1]
  epsilon = t.epsilon or var(1,#a2) * t.cohen
  -- main worker
  function argmin(lo,hi,out,     min,new,cut)
    min = var(lo,hi)
    for arg = lo+step, hi-step do
      local now, after = x(arg), x(arg+1)
      if      now ~= after 
      then if after - start > epsilon 
	    then if stop - now > epsilon 
	    then if mid(arg+1,hi) - mid(lo,arg) > epsilon 
	      then local new = xpect(lo,arg,hi)
	        if new * trivial < min 
	        then min,cut = new,arg end end end end end end
    if   cut 
    then argmin(lo,   cut, out)
         argmin(cut+1, hi, out) 
    else out[ #out+1 ] = a1[lo] end  
    return out
  end 
  return recurse(1, #a1, {})
end
