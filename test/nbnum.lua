local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
package.path = '../src/?.lua;' .. package.path
local _   = require"tricks"
local oo  = require("tricks").oo
local map = require("tricks").map
local the = require"the"
local Nb  = require("nbnum").Nb
local Egs = require("nbnum").Egs

local go = _.go

local eg={}

function eg.egs(tst,  i)
  i=Egs({"Clndrs", "Volume", "Hp:", "Lbs-", "Acc+","Model", "origin", "Mpg+"})
  print("\nx::"); map(i.cols.x,oo) 
  print("\ny::"); map(i.cols.y,oo) end 

function eg.test(tst)
  Nb:new4file("../etc/data/diabetes.csv") end

go(the, eg, b4)
