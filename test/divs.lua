-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
require "lib"

local ok   = require("ok")
local divs = require("divs")

local function y(x)
  if     x <= 0.3 then return 1
  elseif x <= 0.7 then return 2
  else                 return 3 end end

ok{less=function(   a)
  a={}
  for i=1,100 do a[#a+1] = y( r() ) end
  print(divs(a))
end}
