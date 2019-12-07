-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local Lib=require("lib")
local Some=require("some")
local r, o, same, round = Lib.r, Lib.o, Lib.same, Lib.round

local ok   = require("ok")
local divs = require("divs")
local Tbl = require("tbl")

local function y1(x)
  if     x <= 0.25 then return 1
  elseif x <= 0.5 then return 2
  elseif x <= 0.75 then return 3
  else                 return 4 end end

local function y2(x)
  if     x <= 0.25 then return 1+r()
  elseif x <= 0.5 then return 2+r()
  elseif x <= 0.75 then return 3+r()
  else                 return 4+r() end end

ok{big=function(   a,s)
  a={}
  s=Some.new{most=128}
  for i=1,10^4 do Some.add(s, Lib.round(r()^2,3)) end
  o(Some.divs(s))
end}

ok{less1=function(   a)
  a={}
  for i=1,30 do a[#a+1] = y1( r() ) end
  a= divs(a)
  assert(a[1] == 1)
  assert(a[2] == 2)
  assert(a[3] == 3)
  assert(a[4] == 4)
end}

ok{less2=function(   a)
  a={}
  for i=1,30 do a[#a+1] = y2( r() ) end
  a= divs(a)
end}

ok{auto=function(  s,t)
  t=Tbl.new{file="../data/auto93.csv"}
  for _,col in pairs(t.cols.nums) do
    s=Some.new()
    for _,row in pairs(t.rows) do
      Some.add(s, row.cells[col.pos]) end
    print(col.pos, Lib.oo(Some.divs(s))) 
  end
end}

ok{autos= function(  s,a,d)
  s=Some.new{max=128}
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
  for _,v in pairs(a) do Some.add(s,v) end
  d = Some.divs(s)
  assert(d[1] == 10)
  assert(d[2] == 20)
  assert(d[3] == 30)
end}
