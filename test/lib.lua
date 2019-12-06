-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local Lib=require("lib")

local ok   = require("ok")

ok{lib = function()
  Lib.sort{10,20,5,1,30}
end}
