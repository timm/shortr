local _ = {}

function _.new(at,name)   
  return {at=at or 0, name=name or "", 
          nump=false, indep=false, n=0, 
          has={}, most=0, mode=nil} end

function _.add(i,x)
  if x ~= "?" then
    i.n = i.n + 1
    i.has[x] = 1 + (i.has[x] or 0) 
    if i.has[x] > i.most then 
      i.mode,i.most = x,i.has[x] end end 
   return x end

return _
