-- basic NB. discretized data. class is last thing on each line
local _,the = require"tricks", require"the"
local inc,inc3,has3,lines,push = _.inc, _.inc3, _.has3,_.lines,_.push
local classify,train,test,score, nb
local class, new = _.class, _.new
local Nominal, Ratio = class"Nominal",  class"Ratio"

function Egs:like(i,t,prior)
  local l = prior
  for at,x in pairs(t) do -- gotta check its in "x"
    local col = i.cols.all[at]
    if not col.is_goal then
      l=l * ((x=="?" and 0 or i.has[x] or 0) + the.M*prior)/(col.n + the.M) end end 
  return l end

function Nominal.like(i,x, overall, prior)
 return ((i.has[x] or 0) + the.M*prior)/(overall+ the.M) end 

function Ratio.like(i,x,    var,denom,num) 
  if x < i.mu - 3*i.sd then return 0 end
  if x > i.mu + 3*i.sd then return 0 end
  var   = i.sd^2
  denom = (math.pi*2*var)^.5
  num   = math.exp(1)^(-(x-i.mu)^2/(2*var+0.0001))
  return num/(denom + 10^-64) end


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
