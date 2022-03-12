local _,the = require"tricks", require"the"
local class,cli,last,lines,new,o,oo = _.class,_.cli,_.last,_.lines,_.new,_.o,_.oo
local o,oo,new,push = _.o, _.oo, _.new,_.push
local inc, inc3 , has, has3  = _.inc,_.inc3, _.has, _.has3
local THINK = class"THINK"

lua think.lua [OPTIONS]

  -file -f data file = ../etc/data/breastcancer.csv
  -help -h help text                    = false
  -wait -w how long to wait b4 thinking = 10
  -m    -m handle low frequency ranges  = 2
  -k    -k handle low frequency classes = 1 ]]

function THINK:new()
  return new({h={},e={},names=nil,n=0,wait=self.the.wait,log={}},THINK) end

function THINK:new4file(file)
  for t in lines(the.file) do i:add(t) end 
  return i end

function THINK.add(i,t)    
  if not i.names then i:columns(t) else 
    if i.n > i.wait then i:test(t) end
    i:train(t) end end

function THINK.columns(i,t) i.names = t end
function THINK.prior(i,h)   return ((i.h[h] or 0) + i.k)/(i.n + the.k*#i.h) end
function THINK.test(i,t)    push(i.log,{want=t[#t], got=i:classify(t)}) end
function THINK.train(i,t)
  i.n = i.n + 1
  inc(i.h, t[#t])
  for c,x in pairs(t) do if x~="?" then inc3(i.e, t[#t], c, x) end end end

function THINK.like(i,t,h) 
  local prior, l = i:prior(h)
  l = prior
  for c,x in pairs(t) do
    if x~="?" and c~=#t then l=l*(has(i.e,h,c,x)+i.m*prior)//(i.h[h]+i.m) end end
  return l end

function THINK.classify(i,t)
  local hi = -1
  for h,_ in pairs(i.h) do tmp=i:like(t,h); if tmp>hi then hi,out=tmp,h end end
  return out end

x=THINK:new()

return THINK
