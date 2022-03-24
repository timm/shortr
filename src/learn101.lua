local lib = require"lib"
local has2,has3,inc,inc2,sort = lib.has2,lib.has3,lib.inc,lib.inc2,lib.sort
 
local nb={}
function nb.new() return {
   h={}, nh=0,e={}, n=0, wait=the.wait,
   bests=0,rests=0,best={}, rest={},log=log or {}, cols={}} end

function nb.classify(i,t,use)
  local hi,out = -1
  for h,val in pairs(i.h) do 
    local prior = ((i.h[h] or 0) + the.K)/(i.n + the.K*i.nh)
    local l = prior
    for col,x in pairs(t) do
      if x ~= "?" and i.cols[col].indep then
        l=l*(has3(i.e,col,x,h) + the.M*prior)/((i.h[h] or 0) + the.M) end end 
    if l>hi then hi,out=l,h end end
  return out end

function nb.test(i,t)
  if i.n > the.wait then push(i.log,{want=t[#t], got=nb.classify(i,t)}) end  end

function nb.train(i,t)
  local more, kl = false, t[#t]
  for col,x in pairs(t) do 
    if x ~="?" then 
      more = true
      inc3(i.e, col, x, kl) 
      if col ~= #t then
        inc2(kl==the.goal and i.best or i.rest, col,x) end end end
  if more then
    i.n = i.n + 1
    if not i.h[kl] then i.nh = i.nh + 1 end
    inc(i.h, kl)
    if kl==the.goal then i.bests=i.bests+1 else i.rests=i.rests+1 end end end

function nb.score(i)
  local acc,out=0,{}
  for key,x in pairs(i.log) do if x.want==x.got then acc=acc+1/#i.log end end
  for col,xns in pairs(i.best) do
    for x,b in pairs(xns) do
      local r  = has2(i.rest,col,x)
      local r1 = r/i.rests
      local b1 = b/i.bests
      push(out, {100*(b1^2/(b1+r1))//1, col,x,b,i.bests,r,i.rests}) end end
  return acc, sort(out,down1) end 
 
return function(data, log)
  local i = nb.new()
  for row in items(data) do 
    if   #i.cols == 0
    then i.cols=collect(row,function(j,s) return {name=s,indep=truej~=#row} end)
    else test(i,row); train(i,row) end end 
  return i end
