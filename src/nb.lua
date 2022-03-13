-- basic NB. discretized data. class is last thing on each line
local _,the = require"tricks", require"the"
local inc,inc3,has3,lines,push = _.inc, _.inc3, _.has3,_.lines,_.push
local classify,train,test,score, nb

function nb(file)
  local i = {h={}, nh=0,e={}, names=nil, n=0, wait=the.wait, log={}}
  for row in lines(file) do 
    if not i.names then i.names=row else test(i,row); train(i,row) end end 
  return score(i.log) end

function train(i,t)
  i.n = i.n + 1
  if not i.h[t[#t]] then i.nh = i.nh + 1 end
  inc(i.h, t[#t])
  for col,x in pairs(t) do if x~="?" then inc3(i.e,col,x,t[#t]) end end end

function test(i,t)
  if i.n > i.wait then push(i.log,{want=t[#t], got=classify(i,t)}) end end

function classify(i,t)
  local hi,out = -1
  for h,_ in pairs(i.h) do 
    local prior = ((i.h[h] or 0) + the.K)/(i.n + the.K*i.nh)
    local l = prior
    for col,x in pairs(t) do
      if x ~= "?" and col ~= #t then 
        l=l*(has3(i.e,col,x,h) + the.M*prior)/((i.h[h] or 0) + the.M) end end 
    if l>hi then hi,out=l,h end end
  return out end

function score(log,   n)
  n=0; for _,x in pairs(log) do if x.want==x.got then n=n+1 end end
  return n/#log end 

return nb
