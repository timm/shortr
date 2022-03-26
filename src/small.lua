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

local span={}
function span.new(lo,hi) 
  return {cache={},max=20,lo=lo or 100 , hi=1E31, bins=16, seen={}} end

take 10  into 
function span.add(i,x,y,inc)
  local function covers(lo,hi,news)
    local out, span1, span2 = {}
    for p = 1,#news do
      span1 = news[p]
      if lo >= span1.lo and lo < span1.hi then 
        for q = p+1,#news do
          span2 = news[q]
          if hi >= span2.lo and hi < span2.hi then
            for r = p,q do 
              push(out, news[r]) end end end end end 
    return out end
  for _,old in pairs(i.seen) do -- a,b,c,d

    for _,new1 in pairs(covers(old.lo, old.hi, news)) do -- some is subset of some
      
 10    20
    14         26
    if new.lo >= old.lo and new.hi < old.hi then add all of old into new
    if new.lo <  old.lo and 

    s1 = math.max(0,(old.lo - new.lo)/(old.hi - old.lo))
    s2 = math.max(0,(old.hi- new.hi)/(old.hi - old.lo))
    if old.lo>new.lo and old.hi<new.lo then s3 = 
    
    (o.hi - o.lo)
    (o.hi - o.lo)
    (o.hi - o.lo)

end--                                 old
-- 10,20,30,40,50    n        10         20      30        40       50

-- 11,19,31,38,45,55 o           11    19          31   38      45        55
 --                                  new

os.exit(onTheGo(the,go,b4))
