-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local Lib = require("lib")
local o, r, round, same = Lib.o, Lib.r, Lib.round,Lib.same

local ok=require("ok")
local Some=require("some1")
local Divs=require("divs")

ok{adds100=function(   s,d)
  s = Some.new{most=128}
  for i=1,10^6 do Some.add(s,round(r(),3)) end
  for k,v in pairs(Some.has(s)) do print(v) end
  for i=1,100,10 do print(i, Some.ptile(s,i/100)) end
end}
