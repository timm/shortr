function bsearch(t,want,firstp,  lo,mid,hi,out)
  out, lo, hi = 1, lo or 1, #t
  while lo <= hi do
    mid = (lo + hi)//2;
    if want == t[mid] then 
      out = mid
      if firstp then hi=mid-1 else lo=mid+1 end
    else 
      if want < t[mid] then hi=mid-1 else lo=mid+1 end end end
  return out end 

local key,t = 20,{10,20,20,20,40,50}


lo = bsearch(t,key,true)
hi = bsearch(t,key,false,lo)
print(lo,hi)
