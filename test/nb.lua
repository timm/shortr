local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
package.path = '../src/?.lua;' .. package.path
local _    = require"tricks"
local nb   = require"nb"
local the  = require"the"

local go = _.go

local eg={}
function eg.thing(tst,  abcd) 
  print(nb("../etc/data/breastcancer.csv")) end

go(the, eg, b4)
