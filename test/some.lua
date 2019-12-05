-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local Lib=require("lib")
local o,r=Lib.o,Lib.r

local ok=require("ok")
local Some=require("some")

ok{adds100=function(   s)
  s=Some.new{most=128}
  for i=1,10^7 do Some.add(s,r()) end
  o(Some.divs(s))
end}
