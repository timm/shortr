function bsearch(a,want,firstp,  lo,mid,hi,out)
  out, lo, hi = 1, 1,  #a
  while lo <= hi do
    mid = (lo + hi)//2;
    if want == a[mid] then 
      out = mid
      if firstp then hi=mid-1 else lo=mid+1 end
    else 
      if want < a[mid] then hi=mid-1 else lo=mid+1 end end end
  return out end 

local key,t = 40,{10,20,20,20,40,50}
print(bsearch(t,key,true))
print(bsearch(t,key,false))
