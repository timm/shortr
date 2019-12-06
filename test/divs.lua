-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local Lib=require("lib")
local Some=require("some")
local r, o, same = Lib.r, Lib.o, Lib.same

local ok   = require("ok")
local divs = require("divs")

local function y1(x)
  if     x <= 0.2 then return 1
  elseif x <= 0.4 then return 2
  elseif x <= 0.8 then return 3
  else                 return 4 end end

local function y2(x)
  if     x <= 0.2 then return 1+r()
  elseif x <= 0.4 then return 2+r()
  elseif x <= 0.8 then return 3+r()
  else                 return 4+r() end end

ok{big=function(   a,s)
  a={}
  s=Some.new{most=256}
  for i=1,10^7 do Some.add(s, r()) end
  -- for k,v in pairs(a) do print(k,v) end
  --o(divs(a))
end}

same{less1=function(   a)
  a={}
  for i=1,30 do a[#a+1] = y1( r() ) end
  table.sort(a)
  -- for k,v in pairs(a) do print(k,v) end
  a= divs(a)
  o(a)
  assert(a[1] == 1)
  assert(a[2] == 8)
  assert(a[3] == 14)
  assert(a[4] == 26)
end}

same{less2=function(   a)
  a={}
  for i=1,30 do a[#a+1] = y2( r() ) end
  table.sort(a)
  -- for k,v in pairs(a) do print(k,v) end
  a= divs(a)
  assert(a[1]==1)
  assert(a[2]==9)
  assert(a[3]==16)
  assert(a[4]==21)
end}
