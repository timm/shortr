-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
require "lib"

local ok   = require("ok")
local divs = require("divs")

local function y(x)
  if     x <= 0.2 then return 1
  elseif x <= 0.4 then return 2
  elseif x <= 0.8 then return 3
  else                 return 4 end end

ok{less=function(   a)
  a={}
  for i=1,30 do a[#a+1] = y( r() ) end
  table.sort(a)
  --for k,v in pairs(a) do print(k,v) end
  a= divs(a)
  assert(a[1] == 1)
  assert(a[2] == 8)
  assert(a[3] == 14)
  assert(a[4] == 26)
end}
