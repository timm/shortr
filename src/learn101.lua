local the = require"the"
local lib = require"lib"
local has2,has3,inc,inc2,inc3 = lib.has2,lib.has3,lib.inc,lib.inc2,lib.inc3
local push,sort,collect,items = lib.push,lib.sort,lib.collect,lib.items
local map,down1,rnds,oo       = lib.map,lib.down1,lib.rnds,lib.oo
local new,obj                 = lib.new, lib.obj

local NB=obj"NB"
function NB:new(data, i) 
  i = new(NB,{h={}, nh=0,e={}, n=0, wait=the.wait, log=log or {}, cols=nil}) 
  for row in items(data) do 
    if   not i.cols
    then i.cols= collect(row,function(j,s) return {name=s,indep=j~=#row} end)
    else i:test(row); i:train(row) end end 
  return i end

function NB:test(row)
  if self.n > the.wait then 
    push(self.log,{want=row[#row], got=self:classify(row)}) end end

function NB:train(row)
  local more, kl = false, row[#row]
  for col,x in pairs(row) do 
    if x ~="?" then 
      more = true
      inc3(self.e, col, x, kl)  end end
  if more then
    self.n = self.n + 1
    if not self.h[kl] then self.nh = self.nh + 1 end
    inc(self.h, kl) end end

function NB:classify(t,use)
  local hi,out = -math.huge
  print("")
  for h,val in pairs(i.h) do 
    local prior = ((self.h[h] or 0) + the.K)/(self.n + the.K*self.nh)
    local l = math.log(prior)
    for col,x in pairs(t) do
      if x ~= "?" and self.cols[col].indep then
        l = l + math.log((has3(self.e,col,x,h) + the.M*prior) /
                         ((self.h[h] or 0) + the.M)) 
        print(col,x,h,has3(self.e,col,x,h),l)
        end end 
    if l>hi then hi,out=l,h end
    oo(rnds{prior,l,hi,out}) end
  return out end

function NB:score()
  local acc=0
  for key,x in pairs(self.log) do 
    if x.want==x.got then acc=acc+1/#self.log end end
  return acc,self.log end 

return NB
