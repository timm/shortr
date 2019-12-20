-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local Lib = require("lib")
local o, r, round, same = Lib.o, Lib.r, Lib.round,Lib.same

local ok=require("ok")
local Some=require("some1")
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

all = {}
s   = Some.new{most=32}
f   = w4()
for i=1,80 do
  for x=0,2.5,.5 do 
    local z = f(x) 
    Some.add(s,z)
    all[#all+1] = z end end

function p(n,a) return a[math.floor(#a*n)] end

function r3(x) return math.floor(1000*x)/1000 end

table.sort(all)
print(s.n, #(s._has))
for _,j in pairs{.2,.4,.6,.8,1} do 
  print(j, r3(p(j,all)), 
           r3(p(j,Some.has(s))),
           r3(p(j,all)/ p(j,Some.has(s))))
end

same{adds100=function(   s,d)
  s = Some.new{most=128}
  for i=1,10^6 do Some.add(s,round(r(),3)) end
  for k,v in pairs(Some.has(s)) do print(v) end
  for i=1,100,10 do print(i, Some.ptile(s,i/100)) end
end}
