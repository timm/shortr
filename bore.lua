local help = [[

bore = best or rest
(c) 2022, Tim Menzies, BSD 2-clause license.

USAGE: 
  lua bore.lua [OPTIONS]

OPTIONS: 
  -cohen    F   Cohen's delta              = .35
  -data     N   data file                  = etc/data/auto93.csv
  -Dump         stack dump on assert fails = false
  -Format   s   format string              = %5.2f
  -seed     P   set seed                   = 10019
  -todo     S   start up action (or 'all') = nothing
  -help        show help                  = false

KEY: N=fileName F=float P=posint S=string
]]
local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local _ = require"borelib"
local csv,fmt,map,o,oo,push = _.csv,_.fmt,_,map,_.o,_.oo,_.push
local settings, sort, slots = _.settings, _.sort, _.slots
local thing, things         = _.thing, _.things

local col = _.col
local NUM,SYM={},{}

local is={}
function is.ignorep(x) return x:find":$" end     -- columns to ignore
function is.klassp(x)  return x:find"!$" end     -- symbolic goals to achieve
function is.lessp(x)   return x:find"-$" end     -- number goals to minimize
function is.morep(x)   return x:find"+$" end     -- numeric goals to maximize
function is.nump(x)    return x:find"^[A-Z]" end -- numeric columns
function is.goalp(x)   return morep(x) or lessp(x) or klassp(x) end

function col(k,at,txt,t,   i)
  i = new(k,t)
  i.n,i.at,i.txt,i.has = 0,i.at or 0,i.txt or "",{}
  i.w = is.lessp(i.txt) and 0 or 1
  return i end

local DATA,NUM,SYM={},{},{}
function NUM.new(k,at,txt) return col(k,at,txt,{mu=0,m2=0,sd=0})  end
function NUM.heaven(i,x)   return (i.w - i:norm(x))^2 end
function NUM.norm(i,x)     
  return (i.hi-i.lo)<1E-9 and 0 or (x - i.lo)/(i.hi - i.lo + 1E-9) end
function NUM.add(i,x,    d)      
  if x ~="?" then
    d    = x - i.mu
    i.mu = i.mu + d/i.n
    i.m2 = i.m2 + d*(x - i.mu)
    i.sd = Num.sd0(i)
    if x > i.max then i.max = x end
    if x < i.min then i.min = x end end end

function SYM.new(k,at,txt) return col(k,at,txt,{}) end
function SYM.add(i,x)      i.all[x] = 1 + (i.all[x] or 0) end

function DATA.new(k,t) 
  return new(k,{rows={},cols={},x={},y={}}) end

function DATA.dth(i,t)
  local fun = function(col) return col:heaven(t[col.at]) end
  return (sum(i.y, fun)/#i.y)^.5 end

function DATA.add(i,t)
  for at,name in pairs(t) do
    what= (is.nump(name) and NUM or SYM)(at,name)
    if is.ignorep(x) then
      

local the=settings(help)
math.randomseed(the.seed)
goals={}
for n,word in pairs(row) do
  if is.goalp(word) then
    goal[n] = is.less[(word) and -1 or 1 end end

local it= {names={}, cols={}, nums={},x={}, y={}}

local DATA={}
function DATA:new()
  return new(k,{rows={}, names={}, cols={}, nums={},x={},y={}})
function DATA:load(file)  
for row in csv(the.data) do
  if 0==#it.cols then
    it.names=row
    for n,x in pairs(has) do
      col = push(it.cols,{})
      if not is.ignorep(x) then 
        if is.nump[n] then it.nums[n]=true end
        push(is.goalp(x) and it.y or it.x, col) end end
  else
    for n,col in pairs(it.cols) do
      if num

    

end
