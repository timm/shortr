
function bsearch(a,target,firstp,  lo,mid,hi,result)
  lo,hi = 1,  #a
  result = 1;
  while lo <= hi do
    mid = (lo + hi)//2;
    if target == a[mid] then 
      result = mid;
      if firstp then hi=mid-1 else lo=mid+1 end
    else 
      if target < a[mid] then hi=mid-1 else lo=mid+1 end end end
  return result end 

local key,t = 40,{10,20,20,20,40,50}
print(bsearch(t,key,true))
print(bsearch(t,key,false))
