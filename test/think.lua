local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
package.path = '../src/?.lua;' .. package.path
local  _=require"tricks"
local the=require"the"
local go, ok, oo, eg =  _.go, _.ok, _.oo, {}

local the   = require"the"
local THINK = require"THINK"
local ABCD  = require"ABCD"

function eg.thing(tst,  abcd) 
  abcd = ABCD()
  for _,x in pairs(THINK:new4file(the.file).log) do
     abcd:add(x.want, x.got) end 
  abcd:show()
end

go(the, eg, b4)
