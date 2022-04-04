<a name=top>&nbsp;<br>
<img align=left width=175 src="/docs/head.png"> <img 
src="https://img.shields.io/badge/purpose-se,ai-informational?style=flat&logo=hyper&logoColor=white&color=red"> <img 
src="https://img.shields.io/badge/language-lua-informational?style=flat&logo=lua&logoColor=white&color=yellow"> <img 
src="https://img.shields.io/badge/platform-osx,linux-informational?style=flat&logo=linux&logoColor=white&color=orange"> <a
href="https://github.com/timm/l5/actions/workflows/tests.yml"><img src="https://github.com/timm/l5/actions/workflows/tests.yml/badge.svg"></a> <a 
href="https://zenodo.org/badge/latestdoi/206205826"> <img src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a><br>
<b> <a href="https://github.com/timm/l5/blob/master/LICENSE.md">&copy;2022</a> </b>, Tim Menzies, <timm@ieee.org>
<h1>The Little LUA Learning Library</h1><br clear=all>



```lua
brknbad: explore the world better, explore the world for good.
(c) 2022, Tim Menzies
```



```lua
     .-------.  
     | Ba    | Bad <----.  planning= (better - bad)
     |    56 |          |  monitor = (bad - better)
     .-------.------.   |  
             | Be   |   v  
             |    4 | Better  
             .------.  
```



```lua
USAGE:
  ./bnb [OPTIONS]
```



```lua
OPTIONS:
  -K       -K  manage low class counts     = 1
  -M       -M  manage low evidence counts  = 2
  -best    -B  best set                    = .5
  -bins    -b  max. number of bins         = 16
  -cohen   -c  cohen                       = .35
  -dump    -d  dump stack+exit on error    = false
  -far     -F  how far to go for far       = .9
  -file    -f  file name                   = ../etc/data/auto93.csv
  -goal    -g  goal                        = recurrence-events
  -help    -h  show help                   = false
  -leaves  -l  number of items in leaves   = .5
  -p       -p  coefficient on distance     = 2
  -rest    -R  rest is -R*best             = 4
  -seed    -S  seed                        = 10019
  -some    -s  sample size for distances   = 512
  -todo    -t  start up action             = nothing
  -wait    -w  wait                        = 10]]
```



```lua
local go={}
local cols=nil
function go.one()
  local function order(c,a,b)
    local x,y = a[c], b[c]
    x = x=="?" and -math.huge or x
    y = y=="?" and -math.huge or y
    return x < y end
  local function xys_add(col,t,klass,xys,       x)
    xys = xys or {}
    for _,row in pairs(t) do 
      x = row[col.at]
      if x~="?" then push(xys,{x=x,y=klass}) end end 
    return xys end
  local i=egs.Init(the.file) 
  local bests,rests = egs.bestRest(i)
  for _,col in pairs(i.cols.x) do
    if col.nump then
      local xys = bin.Xys(sort(xys_add(col,rests,false, 
                                xys_add(col,bests,true)),upx),
                          col.at,col.name)
      lib.map(xys,oo)
  end end end
```



```lua
os.exit(lib.onTheGo(the,go,b4))
