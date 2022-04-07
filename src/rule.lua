local R = require
local _,the,COLS,BIN,NUM         = R"lib", R"the", R"cols", R"bin", R"num"
local o,oo,down1,map,push,sort,powerset = _.o,_.oo,_.down1,_.map,_.push,_.sort,_.powerset
local slice,merge,slots =  _.slice, _.merge,_.slots
local class,OBJ              = _.class, _.OBJ

local RULE = class("RULE",OBJ)

function RULE.best(bins,h)
   local function score1(b1,b2) return RULE({b1},h).score > RULE({b2},h).score end
   return slice(sort(bins, score1), 1, the.beam) end 
  
function RULE.fromBins(bins,h,   n,out,rule,sizes,scores)
  out={}
  sizes=NUM()
  scores=NUM()
  for _,some in pairs(powerset(RULE.best(bins,h))) do
    if #some>0 then
      rule = RULE(some,h) 
      sizes:add(#some)
      scores:add(rule.score)
      push(out, {size=#some,score=rule.score,rule=rule}) end end
 local function order(one) 
   return ((0 - sizes:norm(one.size))^2 + (1 - scores:norm(one.score))^2)^.5 end
  out = slice(sort(out,function(a,b) return order(a) < order(b) end),1,the.beam)
  for _,three in pairs(out) do 
    print(three.score, three.size, three.rule) end
  return out end 

  -- 3  {-0.66967110414431 RULE{:bins {:2 {BIN{:at 2 :hi 108 :lo -inf :name Volume :ys SYM{:at 0 :has {:left 19 :right 13} :indep true :mode left :most 19 :n 32 :name  :w 1}}}} :score -0.66967110414431 :seen {:left {:2 19} :right {:2 13}}}}
  -- 4  {-0.96579945994854 RULE{:bins {{BIN{:at 1 :hi 6 :lo -inf :name Clndrs :ys SYM{:at 0 :has {:left 19 :right 37} :indep true :mode right :most 37 :n 56 :name  :w 1}}}} :score -0.96579945994854 :seen {:left {19} :right {37}}}}
  -- 5  {-0.97977068533578 RULE{:bins {:7 {BIN{:at 7 :hi 3 :lo 3 :name origin :ys SYM{:at 0 :has {:left 11 :right 9} :indep true :mode left :most 11 :n 20 :name  :w 1}}}} :score -0.97977068533578 :seen {:left {:7 11} :right {:7 9}}}}
  -- 6  {-1.0240113472546 RULE{:bins {:6 {BIN{:at 6 :hi inf :lo 79 :name Model :ys SYM{:at 0 :has {:left 11 :right 11} :indep true :mode right :most 11 :n 22 :name  :w 1}}}} :score -1.0240113472546 :seen {:left {:6 11} :right {:6 11}}}}
  -- 7  {-1.279073376322 RULE{:bins {:7 {BIN{:at 7 :hi 2 :lo 2 :name origin :ys SYM{:at 0 :has {:left 7 :right 8} :indep true :mode right :most 8 :n 15 :name  :w 1}}}} :score -1.279073376322 :seen {:left {:7 7} :right {:7 8}}}}
  -- 8  {-1.8826371927774 RULE{:bins {:6 {BIN{:at 6 :hi 79 :lo -inf :name Model :ys SYM{:at 0 :has {:left 8 :right 48} :indep true :mode right :most 48 :n 56 :name  :w 1}}}} :score -1.8826371927774 :seen {:left {:6 8} :right {:6 48}}}}
  -- 9  {-3.4837551504235 RULE{:bins {:7 {BIN{:at 7 :hi 1 :lo 1 :name origin :ys SYM{:at 0 :has {:left 1 :right 42} :indep true :mode right :most 42 :n 43 :name  :w 1}}}} :score -3.4837551504235 :seen {:left {:7 1} :right {:7 42}}}}
  -- 10  {-4.1245662484181 RULE{:bins {{BIN{:at 1 :hi inf :lo 6 :name Clndrs :ys SYM{:at 0 :has {:right 22} :indep true :mode right :most 22 :n 22 :name  :w 1}}}} :score -4.1245662484181 :seen {:left {0} :right {22}}}}
  -- 11  {-4.6372270804193 RULE{:bins {:2 {BIN{:at 2 :hi inf :lo 108 :name Volume :ys SYM{:at 0 :has {:right 46} :indep true :mode right :most 46 :n 46 :name  :w 1}}}} :score -4.6372270804193 :seen {:left {:2 0} :right {:2 46}}}}
  --
function RULE:new(bins,h,   t)
  self.seen={}
  self.bins = {}
  for _,bin in pairs(bins) do 
    self.bins[bin.at] = self.bins[bin.at] or {}
    push(self.bins[bin.at],  bin) end 
  for _,one in pairs(self.bins) do sort(one, function(a,b) return a.lo < b.lo end) end
  self.score = self:scored(h) 
  end
function RULE:__tostring() return self:show(self.bins)  end --return self:show(self.bins) end

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


RULE.bias = {}
local bias = RULE.bias
function bias.optimize(b,r) return b+r==0 and 0 or b^2/(b+r) end
function bias.monitor( b,r) return b+r==0 and 0 or r^2/(b+r) end
function bias.tabu(    b,r) return b+r==0 and 0 or 1/(b+r) end

function RULE:scored(h)
  return self.bias[the.rule](self:like("left",h), self:like("right",h)) end

function RULE:selects(row)
  local function ors(bins)
    for key,x in pairs(bins) do if bin.select(x,row) then return true end end
    return false end
  for at,bins in pairs(i.bins) do if not ors(bins) then return false end end
  return true end 

function RULE:show(ands)
  local cat, order, sortor, sortand
  cat    = function(t,sep) return table.concat(t,sep) end
  sortand= function(t) return map(slots(t) ,function(k) return t[k] end) end
  sortor = function(a,b)  return a.lo < b.lo end
  return cat(map(sortand(ands),
          function(and1) 
           return cat(map(sort(and1,sortor), 
                   function(or1) return tostring(or1) end)," or ") end)," and ")
end
  --sort(bins,order)
  -- ors=   function(bins) 
  --         return cat(map(merge(sort(bins,order),BIN.mergeNext))," or ") end
  -- return cat(map(bins, ors)," and ") end

return RULE
