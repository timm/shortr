local the = require"the"
local lib = require"lib"
local has2,has3,inc,inc2,inc3 = lib.has2,lib.has3,lib.inc,lib.inc2,lib.inc3
local push,sort,collect,items = lib.push,lib.sort,lib.collect,lib.items
local map,down1,rnds,oo               = lib.map,lib.down1,lib.rnds,lib.oo
 
local nb={}
function nb.new() return {
   h={}, nh=0,e={}, n=0, wait=the.wait, log=log or {}, cols=nil} end

function nb.classify(i,t,use)
  local hi,out =  -math.huge
  print("")
  for h,val in pairs(i.h) do 
    local prior = ((i.h[h] or 0) + the.K)/(i.n + the.K*i.nh)
    local l = math.log(prior)
    for col,x in pairs(t) do
      if x ~= "?" and i.cols[col].indep then
        l=l + math.log((has3(i.e,col,x,h) + the.M*prior)/((i.h[h] or 0) + the.M)) 
        print(col,x,h,has3(i.e,col,x,h),l)
        end end 
    if l>hi then hi,out=l,h end
    oo(rnds{prior,l,hi,out}) end
  return out end

function nb.test(i,t)
  if i.n > the.wait then push(i.log,{want=t[#t], got=nb.classify(i,t)}) end  end

function nb.train(i,t)
  local more, kl = false, t[#t]
  for col,x in pairs(t) do 
    if x ~="?" then 
      more = true
      inc3(i.e, col, x, kl)  end end
  if more then
    i.n = i.n + 1
    if not i.h[kl] then i.nh = i.nh + 1 end
    inc(i.h, kl)
    end end

function nb.score(i)
  local acc=0
  for key,x in pairs(i.log) do if x.want==x.got then acc=acc+1/#i.log end end
  return acc,i.log end 
 
function nb.learn(data, log)
  local i = nb.new()
  for row in items(data) do 
    if   not i.cols
    then i.cols=collect(row, function(j,s) return {name=s,at=j,indep=j~=#row} end)
         oo(i.cols)
    else nb.test(i,row); nb.train(i,row) end end 
  return i end

return nb
