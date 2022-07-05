# [:high_brightness: SHORTr (less, but better, XAI)](all.md)

<a href="all.md"><img align=right width=350 src="shortr.png"></a>

AI and XAI (explainable artificial intelligence) need not be complicated.
For example, here we need just a few 100 lines of LUA to search
N items to  find and explain the best ones, using just log(N) evals. Along the way,
the object model we build could also be applied to  many other AI tasks (nearest neighbor,
decision trees, bayes classifiers, etc).



|       what | where                                                                                                         |
|-----------:|---------------------------------------------------------------------------------------------------------------|
| start here | [[help](all.md)] [[install](/INSTALL.md)] [[design notes](design.md)]    |                                                                 |
| start here | [[help](all.md) &hellip;  [install](/INSTALL.md) &hellip; [design notes](design.md)     |                                                                 |
|      build | [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile) (just for doco)                       |
|      demos | [go](go.md)                                                                                                   |
|       apps | [nb](nb.md) &hellip; [tree](tree.md)                                                                                |
|  functions | [lib](lib.md)                                                                                                 |
|    methods | [bin](bin.md) &hellip; [cols](cols.md) &hellip; [num](num.md) &hellip; [row](row.md) &hellip; [rows](rows.md) &hellip; [some](some.md) &hellip; [sym](sym.md) &hellip; [tree](tree.md) |

<br clear=all>
<p align=center>
<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
</p>

## `SOME` : keep some nums
Given a finite buffer of some small size `max`, then after reading 
a very large set of `n` numbers, we should only be keeping `max/n` of those nums.

Requires :



```lua
local all=require"all"
local obj,push,R,sort,the= all.obj, all.push, all.R, all.sort, all.the
```


> ***SOME(`max` :?int) :SOME***&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; :speech_balloon:  Constructor of a collector for, at most, `max` numbers.  



```lua
local SOME = obj("SOME", function(i,max) 
  i.kept, i.ok, i.max, i.n = {}, true, max, 0  end)
```


> ***add(`i` :`SOME` : `x` :num)***&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; :speech_balloon:    
If full then at odds `i.some/i.n`, keep `x`(replacing some older item, at random).
Otherwise, just add.



```lua
function SOME.add(i,x)
  if x ~= "?" then 
    i.n = i.n + 1
    if #i.kept < i.max     then i.ok=false; push(i.kept,x) 
    elseif R() < i.max/i.n then i.ok=false; i.kept[R(#i.kept)]=x end end end 
```


> ***has(`i` :SOME):tab***&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; :speech_balloon:  Ensure contents are sorted. Return those contents.  



```lua
function SOME.has(i)
  i.kept = i.ok and i.kept or sort(i.kept); i.ok=true; return i.kept ; end
```


That's all folks.



```lua
return SOME
```


