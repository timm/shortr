# [:high_brightness: SHORTr : less (but better) XAI](all.md)

<a href="all.md"><img align=right width=550 src="xai4.png"></a>

AI and XAI (explainable artificial intelligence) need not be
complicated.  For example, here we need just a few 100 lines of LUA
to search N items to  find and explain the best ones, using just
log(N) evals.  
Along the way, we built an object model that could
also be applied to  many other AI tasks (nearest neighbor, decision
trees, bayes classifiers, etc).

**start here**  ([help](all.md) ([install](/INSTALL.md) ([design notes](design.md))))                                                                                               
**build**       ([Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile)-- just for doc)                                                                           
**demos**       ([go](go.md))                                                                                                                                                      
**apps**         ([nb](nb.md) ([tree](tree.md)))   
**functions**   ([lib](lib.md))     
**methods**    ([bin](bin.md) ([cols](cols.md) ([num](num.md) ([row](row.md) ([rows](rows.md) ([some](some.md) ([sym](sym.md) ([tree](tree.md)))))))))


Also, this code shows just how cool is  LUA.
With very little coding, 
the code supports  TDD,  OO, several tiny DSLs,
literate programming, 
command-line
arguments,  and numerous  higher-order tricks. LUA is LISP without brackets!!

<p align=center>
<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
</p>

## Options
For convenience, this code first loads everything from `lib`, then
adds in `the` config table. This means that anyone loading `all`
also can get to all the `lib`.



```lua
local all=require"lib"
```


The `all.opts` function (used here) parses the string to extract
key/value pairs such as  `Min=.5` or `bins=16`. FYI, the `cli`
function (called at start-up in the [go](go.md) file),  checks
for for updates for those keys from the command-line. 



```lua
all.the = all.opts( [[

SHORTr: semi-supervised multi-objective optimization XAI
(c) 2022 Tim Menzies <timm@ieee.org> BSD2 license
     
From N items, find and explain the best ones, using just log(N) evals.
PASS1 (guess): eval two distant items on multi-objective criteria.
      Prune everything nearest the worst one. Recurse on rest.  
PASS2 (guess again): do it again, using better items from first pass.  
PASS3 (explain): recursively discretize attributes on how well they
      distinguish the best and worst items (seen in second pass).
   
USAGE:
  lua go.lua [OPTIONS]
   
OPTIONS:
  -M  --Min    min size of space                    =  .5
  -b  --bins   max number of bins                   =  16
  -F  --Far    how far to look for remove points    =  .95
  -k  --k      Bayes hack: low attribute frequency  =  2
  -m  --m      Bayes hack: low class frequency      =  1
  -p  --p      distance coefficient (2=Euclidean)   =  2
  -s  --seed   random number seed                   =  10019
  -S  --Some   max number of nums to keep           =  256
  -w  --wait   wait this number before testing      =  10
   
OPTIONS (other):
  -f  --file   file           =  ../../data/auto93.csv
  -g  --go     start-up goal  =  nothing
  -h  --help   show help      =  false ]])
```


That's all folks



```lua
return all
```


