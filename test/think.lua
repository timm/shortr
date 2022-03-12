local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
package.path = '../src/?.lua;' .. package.path
local  _=require"tricks"
local the=require"the"
local go, ok, cli, oo, eg =  _.go, _.ok, _.cli, _.oo, {}

oo(the)

local THINK=require"THINK"

function eg.thing(tst) 
  THINK:new4file(the.file)
end

go(eg, b4)
