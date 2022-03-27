local b4={}; for k,v in pairs(_ENV) do b4[k]=v end
local lib,see     = require"lib", require"seen"
local push, items = lib.push, lib.items
local the         = lib.settings([[

brknbad: explore the world better, explore the world for good.
(c) 2022, Tim Menzies

     .-------.  
     | Ba    | Bad <----.  planning= (better - bad)
     |    56 |          |  monitor = (bad - better)
     .-------.------.   |  
             | Be   |   v  
             |    4 | Better  
             .------.  

USAGE:
  ./bnb [OPTIONS]

OPTIONS:]], {
  {"K"     , "-K", "manage low class counts   " , 1},
  {"M"     , "-M", "manage low evidence counts" , 2},
  {"best"  , "-B", "best set                  " , .5},
  {"bins"  , "-b", "max. number of bins       " , 16},
  {"cohen" , "-c", "cohen                     " , .35},
  {"dump"  , "-d", "dump stack+exit on error  " , false},
  {"far"   , "-F", "how far to go for far     " , .9},
  {"file"  , "-f", "file name                 " , "../etc/data/breastcancer.csv"},
  {"goal"  , "-g", "goal                      " , "recurrence-events"},
  {"help"  , "-h", "show help                 " , false},
  {"leaves", "-l", "number of items in leaves " , .5},
  {"p"     , "-p", "coefficient on distance   " , 2},
  {"rest"  , "-R", "rest is -R*best           " , 4},
  {"seed"  , "-S", "seed                      " , 10019},
  {"some"  , "-s", "sample size for distances " , 512},
  {"todo"  , "-t", "start up action           " , nothing},
  {"wait"  , "-w", "wait                      " , 10}})

local go={}
-- end--                                 old
-- -- 10,20,30,40,50    n        10         20      30        40       50
-- -- 11,19,31,38,45,55 o     9      11    19          31   38      45        55
--  --                                  new

function span.new() return {lo=1E31, hi=-1E31, all=nil} end

local function lohi(t,  lo,hi)
  lo,hi = 1E31,-1E31
  for _,one in pairs(t) do
    lo = math.min(lo, one.x)
    hi = math.max(hi, one.x) end
  return lo,hi end

function aa()
  b4  = (i.all[#i.all].hi - i.all[1].lo)/256
  now = (hi - lo)/b4
  for _,one in pairs(i.all) do
  w 
    for j=one.lo, one.hi, (one.hi - one.lo)/16 do
      t[j] = {}
      for k,n in pairs(one.has) do
        t[j][k] = (t[j][k] or 0) + n/16 end end end end
  
    10   14   18   22    26   30
  9      14     19      25    30
--    4   4   4   4   4   4   4
-- 12123232234423455532234455532222

function span.add(i, x,y,inc)
  push(i.cache,{x=x,y=y,inc=inc or 1})
  lo0,hi  = i.all[1].lo
  hi0 = i.all[#i.all].hi
  lo, hi = lo0, hi0
  if #i.cache> i.max then
    for _,xy in pairs(i.cache) do
      lo = math.min(lo,xy.x)
      hi = math.min(hi,xy.x) end 
    if lo < lo0 or hi > hi0 then
      news={}
      gap = (hi-lo)/the.bins
      for j=1,the.bins do
        push(news,{lo=lo,hi=lo+gap,has={}})
        lo=lo+gap end 
      span.Spred(olds,news) end end end
      
function span.spread(i,news)
  for _,old in pairs(olds) do
    for _,new in pairs(news) do
      d, gap = 0, old.hi - old.lo
      if     old.lo>=new.lo and old.hi< new.hi then d= gap
      elseif old.hi>=new.lo and old.lo< new.lo then d= old.hi - new.lo
      elseif old.lo< new.hi and old.hi>=new.hi then d= new.hi - old.lo
      end
      for k,v in pairs(old.has) do
        new.has[k] = v*d/gap + (new.has[k] or 0) end end end
  return news end

function go.one() 
  olds={{lo=10,hi=20,has={y=10,n=20}},
        {lo=20,hi=30,has={y=10,n=5}},
        {lo=30,hi=40,has={y=100,n=100}}}
  
os.exit(lib.onTheGo(the,go,b4))
