-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local Lib = require("lib")
local o, r, round, same = Lib.o, Lib.r, Lib.round,Lib.same

local ok=require("ok")
local Some=require("some")
local Divs=require("divs")

same{adds100=function(   s,d)
  s = Some.new{most=128}
  for i=1,30 do Some.add(s,round(r(),3)) end
  d= Divs.new(s.has)
  assert(d[1]==.006)
  assert(d[2]==.262)
  assert(d[3]==.42)
  assert(d[4]==.572)
end}
