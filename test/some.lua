-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local Lib = require("lib")
local Rand = require("rand")
local abs,o, r, round = Lib.abs, Lib.o, Lib.r, Lib.round
local within, same = Lib.within, Lib.same

local ok=require("ok")
local Some=require("some")
local Divs=require("divs")

local function xx(n) return {1,1,1,n,1} end

ok{div100=function(   s,d)
  s = Some.new{most=256,key=function(z) return z[4] end}
  for i=1,100 do Some.add(s,xx(round(r(),3))) end
  d= Some.divs(s)
  Lib.map(d,o)
  assert(within(0.19,d[1].hi,0.21))
  assert(within(0.79,d[5].hi,0.81))
end}


ok{div1002=function(   s,d)
  s = Some.new{most=256}
  for i=1,100 do Some.add(s,round(r()^2,3)) end
  d= Some.divs(s)
  Lib.map(d,o)
  assert(within(0.11,d[1].hi,0.13))
  assert(within(0.66,d[3].hi,0.67))
end}

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
local function p(n,a) return a[math.floor(#a*n)] end

local function r4(x) local n=10^4; return math.floor(n*x)/n end
local function p4(x) return 100 *r4(x) end

local function go(ii,m)
  print("")
  local all = {}
  m   = m or 256 -- keep this above 255. but it doesn't seem to matter how high we go
  local s1  = Some.new{most=m}
  local f   = w4()
  for i=1,ii do
    for x=0,2.5,.01 do 
      local z = f(x) 
      Some.add(s1,z)
      all[#all+1] = z end end

  table.sort(all)
  local e1=0
  local e1s=0
  local r=0
  for _,j in pairs{.1,.2,.3,.4,.5,.6,.7,.8,.9} do 
        e1= abs(p(j,Some.has(s1))-p(j,all))/ p(j,all)
        e1s = e1s+e1
        r=r+1
        print(j, r4(p(j,all)), r4(p(j,Some.has(s1))), p4(e1))
  end
  print(ii,m,"av.err=", p4(e1s/r))
end

--for ii =1.1,10 do
local ii=5
print("")
--go(ii,32)
--go(ii,64)
--go(ii,128)
go(ii,256)
--go(ii,512)
--go(ii,1024)

ok{adds100=function(   s,d)
  s = Some.new()
  for i=1,10^3 do Some.add(s,round(r(),3)) end
  -- for k,v in pairs(Some.has(s)) do print(v) end
  for i=10,90,10 do print(i, Some.ptile(s,i/100)) end
end}


