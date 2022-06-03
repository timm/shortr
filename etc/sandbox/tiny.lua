THE={bins=16}
fmt=string.format
fmtp= function(...) print(fmt(...)) end 
rand=math.random
function bool(x) 
  if x==nil 
  then return (rand()<.5 and true or false) 
  else return type(x)==boolean end end

function bool(x) 
  if x==nil 
  then return (rand()<.5 and true or false) 
  else return type(x)==boolean end end

function normal(mu,sd) 
  return mu + sd*math.sqrt(-2*math.log(R()))*math.cos(2*math.pi*R()) end

function normpdf(x, mu, sd,      var,denom,num)
  var = sd^2
  denom = (2*math.pi*var)^.5
  num    = math.exp(-(x-mu)^2/(2*var)) 
  return num/denom end

function rnd(n, p)   local m=10^(p or 0); return math.floor(n*m+0.5)/m  end

function _chops(b)
  p,t=0,{}
  for x=-3,3,6/b do
    p=p + normpdf(x,0,1) 
    t[1+#t] = p*6/b  end 
  t[#t]=1
  return t end


_chop={}
function chop(x,b)
  chops[b] = chops[b] or _chops(b)
  x= normpdf(x,mu,sd)
  for k,v in pairs(chops[b)) do if v>=x then return k end


chops,chopsm={},{}
chopsm.__index =  function (t, k) return _chops(k) end
setmetatable(chops,chopsm)
for k,v in pairs(chops[7]) do print(k,v) end
