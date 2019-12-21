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
     return w(x,1,k1) end -- w(x,1,k2) + w(x,1,k3) + w(x,1,k4) end 
end

all = {}
m   = 128
s0  = Some0.new{most=m}
s1  = Some1.new{most=m}
f   = w4()
for i=1,100 do
  for x=0,2.5,.01 do 
    local z = f(x) 
    Some0.add(s0,z)
    Some1.add(s1,z)
    all[#all+1] = z end end

function p(n,a) return a[math.floor(#a*n)] end

function r4(x) local n=10^4; return math.floor(n*x)/n end
function p4(x) return 100 *r4(x) end

table.sort(all)
print(0,s0.n, #(s0.has))
print(1,s1.n, #(s1._has))
table.sort(s0.has)
for _,j in pairs{.1,.2,.3,.4,.5,.6,.7,.8,.9,1} do 
  print(j, r4(p(j,all)), 
           r4(p(j,s0.has)),
           r4(p(j,s1._has)),
           p4(abs(p(j,s0.has)-p(j,all))/ p(j,all)),
           p4(abs(p(j,Some1.has(s1))-p(j,all))/ p(j,all)))
end

same{adds100=function(   s,d)
  s = Some.new{most=128}
  for i=1,10^6 do Some.add(s,round(r(),3)) end
  for k,v in pairs(Some.has(s)) do print(v) end
  for i=1,100,10 do print(i, Some.ptile(s,i/100)) end
end}
