-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local Lib=require("lib")
local ok   = require("ok")

local Object=require("object")
local Some=require("some")
local Divs2 = require("divs2")
local Tbl = require("tbl")

local r, o, same, round = Lib.r, Lib.o, Lib.same, Lib.round

local function y1(x)
  x = round(x,3)
  if     x <= 0.25 then return {x,1}
  elseif x <= 0.5  then return {x,2}
  elseif x <= 0.75 then return {x,3}
  else                  return {x,4} end end

local function y2(x)
  x = round(x,3)
  if     x <= 0.25 then return {x,1+r()}
  elseif x <= 0.5 then return {x,2+r()}
  elseif x <= 0.75 then return {x,3+r()}
  else                 return {x,4+r()} end end

local function first(a) return a[1] end
local function second(a) return a[2] end

function big(   a,s,m,d)
  a,m = {},10^5
  for i=1,m do a[#a+1] = y1(i/m) end
  d= Divs2.new(a,{fx=first,fy=second})
  for _,x in pairs(d) do o(x) end
  assert(d[1]==0.012)
  assert(d[2]==0.251)
  assert(d[3]==0.505)
  assert(d[4]==0.763)
end
big()

ok{big=big}

same{less1=function(   a,m,d)
  a,m = {},10^4
  for i=1,m do a[#a+1] = y2( i/m ) end
  d= Divs2.new(a,{fx=first, fy=second})
  assert(d[1]==0.0)
  assert(d[2]==0.258)
  assert(d[3]==0.507)
  assert(d[4]==0.775)
end}

same{autos= function(  a,d)
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
  d= Divs2.new(a,{x=same})
  assert(d[1]==10)
  assert(d[2]==20)
  assert(d[3]==30)
  assert(d[4]==40)
end}
