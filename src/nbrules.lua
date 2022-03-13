-- basic NB. discretized data. class is last thing on each lone
local _,the = require"tricks", require"the"
local copy,inc,inc3,has3, lines = _.copy, _.inc, _.inc3, _.has3, _.lines
local o,oo,push,inc, inc3  = _.o,    _.oo,  _.push, _.inc,  _.inc3
local map,sort,firsts, stsrif = _.map,_.sort, _.firsts,_.stsrif

local function classify(i,t)
  local hi,out = -1
  for h,_ in pairs(i.h) do 
    local prior = ((i.h[h] or 0) + the.K)/(i.n + the.K*i.nh)
    local l = prior
    for col,x in pairs(t) do
      if x ~= "?" and col ~= #t then 
        l=l*(has3(i.e,col,x,h) + the.M*prior)/((i.h[h] or 0) + the.M) end end 
    if l>hi then hi,out=l,h end end
  return out end

local function train(i,t)
  i.n = i.n + 1
  if not i.h[t[#t]] then i.nh = i.nh + 1 end
  inc(i.h, t[#t])
  for col,x in pairs(t) do 
    if x~="?" then inc3(i.e,col,x,t[#t]) end end end

local function test(i,t)
  if i.n > i.wait then push(i.log,{want=t[#t], got=classify(i,t)}) end end

local function rank(i,dodge)
  local out,funs={},{}
  function funs.xplore(b,r)  return 1 / (b+r) end
  function funs.refute(b,r)  return 1 - abs(b-r) end
  function funs.plan(b,r)    return b<r and 0 or b^2/(b+r) end
  function funs.monitor(b,r) return r<b and 0 or r^2/(b+r) end
  print(#i.e)
  for col,xs in pairs(i.e) do
    if col ~= dodge then
      for x,hs in pairs(xs) do  
        local b, r, B, R = 0, 0, 1E-32, 1E-32
        for h,n in pairs(hs) do 
          if   h==the.goal 
          then B = B + i.h[h]; b = b+n 
          else R = R + i.h[h]; r = r+n end end
      push(out,{funs[the.want](b/B, r/R), {col=col,val=x}}) end end end
  return sort(out,stsrif) end

local function nb(file,    i)
  i = {h={}, nh=0,e={}, names=nil, n=0, wait=the.wait, log={}}
  for row in lines(file) do 
    if not i.names then i.names=row else test(i,row); train(i,row) end end 
  map(rank(i,#i.names),oo)
  return i end

return {nb=nb}
