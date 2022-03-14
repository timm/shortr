local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
package.path = '../src/?.lua;' .. package.path
local _    = require"tricks"
local go,lines,ok,oo = _.go, _.lines, _.ok, _.oo
local Egs = require("egs").Egs
local the = require"the"
local eg  = {}

function eg.two(tst,  t,header,kl) 
  t={}
  for row in lines("../etc/data/breastcancer.csv") do
    if not header then header=row else
      kl = row[#row]
      t[kl] = t[kl] or Egs(header) 
      t[kl]:add(row) end end 
  for _,kl in pairs(t) do oo(kl:mid()) end end

go(the, eg, b4)
