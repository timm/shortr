local the,help = {},[[
brknbad.lua: explore the world better, explore the world for good.
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

OPTIONS:
  -bins  -b   max. number of bins            = 16
  -best  -B   best set                       = .5
  -rest  -R   rest is -R*best                = 4
  -cohen -c   cohen                          = .35
  -goal  -g   goal                           = recurrence-events
  -K     -K   manage low class counts        = 1
  -M     -M   manage low evidence counts     = 2
  -seed  -S   seed                           = 10019
  -wait  -w   wait                           = 10

OPTIONS (other):
  -dump  -d   dump stack on error, then exit = false
  -file  -f   file name                      = ../etc/data/breastcancer.csv
  -help  -h   show help                      = false
  -todo  -t   start up action                = nothing
]]

local function cli(long,key,short,x)
  local function thing(x)
    if type(x) ~="string" then return x end
    x = x:match"^%s*(.-)%s*$"
    if x=="true" then return true elseif x=="false" then return false end
    return tonumber(x) or x end 
  local used={}
  assert(not used[short], "repeated short flag ["..short.."]")
  used[short]=short
  for n,flag in ipairs(arg) do 
    if flag==short or flag==long then
      x = x=="false" and true or x=="true" and "false" or arg[n+1] end end 
   the[key] = thing(x) end

help:gsub("\n  ([-]([^%s]+))[%s]+(-[^%s]+)[^\n]*%s([^%s]+)",cli)
if the.help then os.exit(print(help)) end
return the
