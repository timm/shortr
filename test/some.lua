-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local Lib = require("lib")
local o, r, round = Lib.o, Lib.r, Lib.round

local ok=require("ok")
local Some=require("some")

ok{adds100=function(   s,t,u)
  s = Some.new{most=128}
  for i=1,10^5 do Some.add(s,r()) end
  t = Some.divs(s)
  o(t)
  u = {}
  for _,k in pairs(t) do u[#u+1] = round(s.has[k],3) end
  o(u)
end}
