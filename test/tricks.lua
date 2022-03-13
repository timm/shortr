local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
package.path = '../src/?.lua;' .. package.path
local  _   = require"tricks"
local  the = require"the"
local go, ok, cli, eg =  _.go, _.ok, _.cli, {}

local last,lines,many,o,oo,per = _.last, _.lines, _.many, _.o, _.oo, _.per 
local powerset = _.powerset
local push,shuffle,sum,things = _.push, _.shuffle, _.sum,  _.things

function eg.last(tst) 
  ok(tst, 30 == last{10,20,30}, "lasts") end

function eg.per(tst,  t)
  t={};for i=1,100 do push(t,i*1000) end
  ok(tst,70000 == per(t,.7), "per") end

function eg.many(tst,  t)
  t={};for i=1,100 do push(t,i) end; many(t,10) end

function eg.sum(tst,   t) 
  t={};for i=1,100 do push(t,i) end; ok(tst,5050==sum(t),"sum")end

function eg.shuffle(tst, t, good)
  t={1,2,3,4,5,6,7,8,9}
  good = true
  for j=1,10^5 do 
    t= shuffle(t); 
    good = good and sum(t)==45,"shuffle "..j end 
  ok(tst,good, "shuffling") end

function eg.powerset(tst, t)
  t = {10,20,30,40}
  for _,x in pairs(powerset(t)) do oo(x) end end

-- function eg.lines(tst)
--   for t in lines("tricks.lua", things) do print(#t, o(t)) end end

go(the, eg, b4)
