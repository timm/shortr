-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Lib = require("lib")
local Abcd= require("abcd")

local function _abcd()
  local a = Abcd()
  for i=1,6 do a:add('y','y') end
  for i=1,2 do a:add('n','n') end
  for i=1,5 do a:add('m','m') end
  a:add('m','n')
  for k,v in pairs(a:report()) do print(k,Lib.oo(v)) end
end
 
_abcd()
