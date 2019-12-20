-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local heap=require("heap")
local l=require("lib")
local ok=require("ok")

local function f()
  return math.floor(math.random()*100) end

function parent(n) return math.floor(n/2) end
function left(n)  return 2*n end
function right(n) return 2*n + 1 end

function bsearch(a,x,get,n,b4,    here)
  get = get or function(a,z) return a[z] end
  n = n or 1
  if not a[n]      then return a[b4] end
  here = get(a[n])
  --print(n,here)
  if here  == x then return a[n] end
  if 
  return bsearch(a,x,get,
                 here > x and right(n) or left(n),
                 n) 
end
  
ok{heap= function( h,has) 
  math.randomseed(1)
  h=heap{cmp = function(a,b)  return a.age < b.age end}
  for i = 20,1,-1 do h:push({age=i}) end
 -- while h:length() > 1 do
   --  print(h:pop().age) end
  has = h:has()
  for i=1,h:length() do 
     print(">",i,has[i].age) end
  x =1
  m = 11
  get = function(z) return z.age end
  for i=1,20 do
    print(">>",i, get(bsearch(has,i,get)))
  end
  --for i=1,#has do print(i, has[i].age) end
end}

l.same{heap1=function(  h,a,n,has)
  h=heap()
  a={6,10,8,12,18,11,25,21,17,19}
  for i=1,#a do h:push(a[i]) end
  n = h:length()
  for i=1,n do print(i,h:has()[i]) end
end}
