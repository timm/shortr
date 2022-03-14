-- A Naive Bayes classifier, that knows numeric and symbolic columns
-- (and the dependent and independent columns can occur in any order).
local _       = require"tricks"
local the     = require"the"
local Egs     = require("Egs").Egs
local Ratio   = require("Egs").Ratio
local Nominal = require("Egs").Nominal
local inc,inc3,has3,lines,push = _.inc, _.inc3, _.has3, _.lines, _.push
local is,as = _.is, _.as
local Nb = is"Nb"

-- ## Add likelihood calculators
function Egs.like(i,t,prior)
  local like = prior
  for at,x in pairs(t) do
    local col = i.cols.all[at]
    if not col.is_goal then
      like = like * (x=="?" and 1 or i.cols.all[at]:like(x,prior)) end end 
  return like end

function Ratio.like(i,x,prior)
  if x < i.mu - 3*i.sd then return 0 end
  if x > i.mu + 3*i.sd then return 0 end
  local denom = (math.pi*2*i.sd^2)^.5
  local nom   =  math.exp(1)^(-(x-mu)^2/(2*i.sd^2+1E-32))
  return nom/(denom + 1E-32) end

function Nominal.like(i,x,prior) 
  return ((i.has[x] or 0) + the.M*prior)/(i.n + the.M) end 

-- ## Create and update
function Nb:new() 
  return {h={}, all=nil, nh=0, n=0, wait=the.wait, log={}} end

function Nb:new4file(file) 
  for row in lines(file) do i:add(row) end end

function Nb.add(i,row)
  if not i.all then i.all = Egs(row) else i:test(row); i:train(row) end end 

-- ## Train, test, classify
function Nb.train(i,t)
  i.n = i.n + 1
  local h = i.all:klass(t)
  if not i.h[h] then i.nh = i.nh + 1; i.h[h] = i.all:clone() end
  i.h[h]:add(row) 
  i.all:add(row) end

function Nb.test(i,t)
  if i.n > i.wait then push(i.log, {want=i.all:klass(t), got=classify(i,t)}) end end

function Nb.classify(i,t)
  local hi,out = -1
  for klass,h in pairs(i.h) do 
    local prior = (h.n + the.K) / (i.n + the.K*i.nh)
    local like  = h:like(t,prior)
    if like > hi then hi,out=like,klass end end
  return out end

-- ## Score
function Nb.score(i,    n)
  n=0; for _,x in pairs(i.log) do if x.want==x.got then n=n+1 end end
  return n/#i.log end 

-- ## Return
return Nb
