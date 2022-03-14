local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
package.path = '../src/?.lua;' .. package.path
local _   = require"tricks"
local the = require"the"
local Nb  = require("nbnum").Nb

local go = _.go

local eg={}
function eg.test(tst)
  Nb:new4file("../etc/data/breastcancer.csv") end

go(the, eg, b4)
