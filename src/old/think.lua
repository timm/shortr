local _,the = require"tricks", require"the"
local class,cli,last,lines,new,o,oo = _.class,_.cli,_.last,_.lines,_.new,_.o,_.oo
local o,oo,new,push = _.o, _.oo, _.new,_.push
local inc, inc3 , has, has3  = _.inc,_.inc3, _.has, _.has3

-- ## Class
local THINK = class"THINK"

function THINK:new()
  return new({h={}, nh=0,e={}, names=nil, n=0, wait=the.wait, log={}},THINK) end

-- ## Creation
function THINK:new4file(f,  i) 
  i = THINK(); for t in lines(f) do i:add(t) end; return i end

function THINK.cols(i,t)   
  i.names = t end

function THINK.add(i,t)    
  if not i.names then i:cols(t) else 
    if i.n > i.wait then i:test(t) end
    i:train(t) end end

-- ## Inference
function THINK.prior(i,h) 
  return ((i.h[h] or 0) + the.K)/(i.n + the.K*i.nh) end

function THINK.like(i,t,h)
  local prior, l = i:prior(h)
  l = prior
  for c,x in pairs(t) do
    if x ~= "?" and c ~= #t then 
      l=l*(has3(i.e,h,c,x) + the.M*prior)/((i.h[h] or 0) + the.M) end end 
  return l end

function THINK.classify(i,t)
  local hi, out, l = -1
  for h,_ in pairs(i.h) do 
    l=i:like(t,h)
    if l>hi then hi,out=l,h end end
  return out end

-- ## Updates
function THINK.test(i,t) 
  push(i.log,{want=t[#t], got=i:classify(t)}) end

function THINK.train(i,t)
  i.n = i.n + 1
  if not i.h[t[#t]] then i.nh = i.nh + 1 end
  inc(i.h, t[#t] )
  for c,x in pairs(t) do if x~="?" then inc3(i.e, t[#t], c, x) end end end


-- ## incremental
function THINK.optimize(i,b,r) return b<=r and 0 or b^2/(b+r+1E-32) end
function THINK.monitor(i,b,r)  return r<=b and 0 or r^2/(b+r+1E-32) end
function THINK.tabu(i,b,r)     return 1/(b+r+1E-32) end

-- ## Returns
return THINK
