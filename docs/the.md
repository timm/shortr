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
  -bins  -b   max. number of bins        = 16
  -best  -B   best set                   = .5
  -cohen -c   cohen                      = .35
  -far   -F   how far to go for far      = .9
  -goal  -g   goal                       = recurrence-events
  -K     -K   manage low class counts    = 1
  -leaves -l  number of items in leaves  = .5
  -M     -M   manage low evidence counts = 2
  -p     -p   coefficient on distance    = 2
  -rest  -R   rest is -R*best            = 4
  -some  -s   sample size for distances  = 512
  -seed  -S   seed                       = 10019
  -wait  -w   wait                       = 10
```



```lua
OPTIONS (other):
  -dump  -d   dump stack on error then quit = false
  -file  -f   file name        = ../etc/data/breastcancer.csv
  -help  -h   show help        = false
  -todo  -t   start up action  = nothing
]]
