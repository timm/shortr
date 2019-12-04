-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local Lib=require("lib")

local ok=require("ok")
local Some=require("some")

ok{adds100=function(   s)
  s=Some.new{most=32}
  for i=1,1000 do Some.add(s,i) end
  assert(Lib.sort(s.has)[16] == 505 )
end}
