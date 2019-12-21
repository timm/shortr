-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local Lib = require("lib")
local abs,o, r, round, same = Lib.abs, Lib.o, Lib.r, Lib.round,Lib.same

local ok=require("ok")
local Some0=require("some")
local Some1=require("some1")
local Divs=require("divs")

local function w(x,lamb,k,  e)
  e = 2.71828
  if x<0 then return 0 end
  return k/lamb*(x/lamb)^(k-1)*e^(-(x/lamb)^k)
end

local function w4(   k1,k2,k3,k4)
  k1=0.5+r()*4.5
  k2=0.5+r()*4.5
  k3=0.5+r()*4.5
  k4=0.5+r()*4.5
  return function(x) 
     return w(x,1,k1) + w(x,1,k2) + w(x,1,k3) + w(x,1,k4) end 
end
function p(n,a) return a[math.floor(#a*n)] end

function r4(x) local n=10^4; return math.floor(n*x)/n end
function p4(x) return 100 *r4(x) end

function go(ii,m)
  local all = {}
  m   = m or 256 -- keep this above 255. but it doesn't seem to matter how high we go
  local s0  = Some0.new{most=m}
  local s1  = Some1.new{most=m}
  local f   = w4()
  for i=1,ii do
    for x=0,2.5,.01 do 
      local z = f(x) 
      Some0.add(s0,z)
      Some1.add(s1,z)
      all[#all+1] = z end end

  table.sort(all)
  table.sort(s0.has)
  local e0,e1=0,0
  local e0s,e1s=0,0
  local r=0
  for _,j in pairs{.1,.2,.3,.4,.5,.6,.7,.8,.9} do 
        e0 = abs(p(j,s0.has)-p(j,all))/ p(j,all)
        e1= abs(p(j,Some1.has(s1))-p(j,all))/ p(j,all)
        e0s = e0s+e0
        e1s = e1s+e1
        r=r+1
        --print(j, r4(p(j,all)), r4(p(j,s0.has)), r4(p(j,Some1.has(s1))), p4(e0), p4(e1))
  end
  print(ii,m, p4(e0s/r), p4(e1s/r))
end

for ii =1.1,10 do
print("")
go(ii,32)
go(ii,64)
go(ii,128)
go(ii,256)
go(ii,512)
go(ii,1024)
end

same{adds100=function(   s,d)
  s = Some.new{most=128}
  for i=1,10^6 do Some.add(s,round(r(),3)) end
  for k,v in pairs(Some.has(s)) do print(v) end
  for i=1,100,10 do print(i, Some.ptile(s,i/100)) end
end}
