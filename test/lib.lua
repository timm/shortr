-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local Lib=require("lib")

local ok   = require("ok")

ok{lib = function()
  Lib.sort{10,20,5,1,30}
end}

ok{copy = function(    a,b)
  a = {m={n=1,o=10}, p={q={r=100},s={t=1000}}}
  b = Lib.copy(a)
  b.p.s.t  = b.p.s.t + 1
  assert(a.p.s.t ~= b.p.s.t)
end}
