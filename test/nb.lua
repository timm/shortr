local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
package.path = '../src/?.lua;' .. package.path
local _    = require"tricks"
local nb   = require"nb".nb
local the  = require"the"
local ABCD = require"ABCD"

local go = _.go

local eg={}
function eg.thing(tst,  abcd) 
  abcd = ABCD()
  for _,x in pairs(nb("../etc/data/breastcancer.csv").log) do
     abcd:add(x.want, x.got) end 
  abcd:show() end

go(the, eg, b4)
