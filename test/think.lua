local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
package.path = '../src/?.lua;' .. package.path
local  _=require"tricks"
local go, ok, cli, eg =  _.go, _.ok, _.cli, {}

local THINK=require"THINK"

function eg.last(tst) 
  ok(tst, 30 == last{10,20,30}, "lasts") end

go(cli(THINK.help), eg, b4)
