-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local Lib=require("lib")
local ok   = require("ok")

local Object=require("object")
local Some=require("some")
local divs2 = require("divs2")
local Tbl = require("tbl")

local r, o, same, round = Lib.r, Lib.o, Lib.same, Lib.round

local function y1(x)
  x = round(x,3)
  if     x <= 0.15 then return {x,1}
  elseif x <= 0.35  then return {x,2}
  elseif x <= 0.55 then return {x,4}
  elseif x <= 0.85 then return {x,5}
  else                  return {x,6} end end

local function y2(x)
  x = round(x,3)
  if     x <= 0.25 then return {x,1+r()}
  elseif x <= 0.5 then return {x,2+r()}
  elseif x <= 0.75 then return {x,3+r()}
  else                 return {x,4+r()} end end

local function first(a) return a[1] end
local function second(a) return a[2] end

local function big(   a,s,m,d)
  a,m = {},10^2
  for i=1,m do a[#a+1] = y1(r()) end
  d= divs2(a,{fx=first,fy=second})
  for n,v in pairs(d) do
     print(n,v.x.lo)
  end
  assert(d[1].x.lo==0.006)
  assert(d[2].x.lo==0.165)
  assert(d[3].x.lo==0.352)
  assert(d[4].x.lo==0.572)
  assert(d[5].x.lo==0.857)
end
big()

ok{big=big}
ok{autos= function(  a,d)
  a={10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
  10, 10, 10, 10, 10, 10, 10, 10, 10, 10 , 10, 10,
  10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
  10, 10, 10, 10, 10, 10, 10, 10 , 10, 10, 10, 10,
  10, 10, 10, 10, 10, 20, 20, 20, 20, 20, 20, 20,
  20, 20, 20, 20, 20, 20 , 20, 20, 20, 20, 20, 20,
  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
  20, 20, 20, 20 , 20, 20, 20, 20, 20, 20, 20, 20,
  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
  20, 20 , 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20 ,
  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
  20, 20, 20, 20, 20, 20, 20, 20, 20, 20 , 20, 20,
  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
  20, 20, 20, 20, 20, 20, 20, 20 , 20, 20, 20, 20,
  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
  20, 20, 20, 20, 20, 20 , 20, 20, 20, 20, 20, 20,
  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
  20, 20, 20, 20 , 20, 20, 20, 20, 20, 20, 20, 20,
  20, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30,
  30, 30 , 30, 30, 30, 30, 30, 30, 30, 30, 30, 30,
  30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30 ,
  30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30,
  30, 30, 30, 30, 30, 30, 30, 30, 30, 30 , 30, 30,
  30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30,
  30, 30, 30, 30, 30, 30, 30, 30 , 30, 30, 30, 30,
  30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30,
  30, 30, 30, 30, 30, 30 , 30, 30, 30, 30, 30, 30,
  30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30,
  30, 30, 30, 30 , 30, 30, 30, 30, 30, 30, 30, 30,
  30, 30, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
  40, 40 , 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
  40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40 ,
  40, 50 }
  d= divs2(a,{x=same})
  assert(d[1].x.lo==10) assert(d[1].x.hi==10)
  assert(d[2].x.lo==20) assert(d[2].x.hi==20)
  assert(d[3].x.lo==30) assert(d[3].x.hi==30)
  assert(d[4].x.lo==40) assert(d[4].x.hi==40)
end}
