local R = require
local _,the,COLS,BIN         = R"lib", R"the", R"COLS", R"BIN"
local o,oo,map,push,sort,powerset = _.o,_.oo,_.map,_.push,_.sort,_.powerset
local class,OBJ              = _.class, _.OBJ

local RULE = class("RULE",OBJ)
function RULE.fromBins(bins,h,   n,out,rule)
  out={}
  for _,some in pairs(powerset(bins)) do
    if #some>0 then
      rule = push(out, RULE(some,h)) ; 
      local n=0; for _,v in pairs(h) do n = n + v end 
      if  rule.score ~= 0 then
         print(rule.score, "|",the.rule, n,o(h),"|",o(map(some,tostring)),o(rule.seen), #some) end
      --
      end end 
  return out end 

function RULE:new(bins,h,   t)
  self.seen={}
  self.bins = {}
  for _,bin in pairs(bins) do 
    self.bins[bin.at] = self.bins[bin.at] or {}
    push(self.bins[bin.at],  bin) end 
   self.score = self:scored(h) 
  end

function RULE:like(klass,h) -- h={"true"=100, "false"=40} n=100+40
  local n=0; for _,v in pairs(h) do n = n + v end 
  local fs = {}
  for at,bins in pairs(self.bins) do
    fs[at] = 0
    for _,bin in pairs(bins) do 
      fs[at] = fs[at] + (bin.ys.has[klass] or 0) end end
  self.seen[klass] = fs
  local prior = ((h[klass] or 0) + the.K) / (n + the.K * 2)
  local out   = math.log(prior)
  for at,v in pairs(fs) do 
     local inc = (v+the.M*prior)/(h[klass]+the.M) 
     out=out + math.log( inc) 
     end
  return out end


local z=-math.huge
RULE.bias = {}
local bias = RULE.bias
function bias.optimize(b,r) return b<r and 0 or b^2/(b+r) end
function bias.monitor( b,r) return r^2/(b+r) end
function bias.tabu(    b,r) return 1/(b+r) end

function RULE:scored(h)
  return self.bias[the.rule](self:like("left",h), self:like("right",h)) end

function RULE:selects(row)
  local function ors(bins)
    for key,x in pairs(bins) do if bin.select(x,row) then return true end end
    return false end
  for at,bins in pairs(i.bins) do if not ors(bins) then return false end end
  return true end 

function RULE:show(bins)
  local cat, order, ors
  cat =  function(t,sep) return table.concat(t,sep) end
  order= function(a,b)  return a.lo < b.lo end
  ors=   function(bins) 
          return cat(map(bin.Merges(sort(bins,order)),bin.show)," or ") end
  return cat(map(i.bins, ors)," and ") end

return RULE
