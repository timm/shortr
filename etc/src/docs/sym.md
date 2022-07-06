
# [:high_brightness: SHORTr : less (but better) XAI](all.md)

<a href="all.md"><img align=right width=400 src="stark.jpeg"></a>

AI and XAI (explainable artificial intelligence) need not be
hard.  E.g. here's a few hundred lines of LUA
to search N items to  find and explain the best ones, using just
log(N) evals.  

**start here:**  ([help](all.md) ([install](/INSTALL.md) ([design notes](design.md))))                                                                                               
**build:**       ([Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile)-- just for doc)                                                                           
**demos:**       ([go](go.md))                                                                                                                                                      
**apps:**         ([nb](nb.md) ([tree](tree.md)))   
**functions:**   ([lib](lib.md))     
**columns:**    ([cols](cols.md) ([num](num.md) ([some](some.md) ([sym](sym.md)))))  
**rows:** ([row](row.md) ([rows](rows.md)))   
**trees:** ([bin](bin.md) ([tree](tree.md))))

<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a><br>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 


## Class SYM
Summarize symbols

**RESPONSIBILITIES** : 
- [Create](#create) a duplicate structure 
- [Discretize](#discretize) numerics into a few bins (for building trees)
- [Distance](#distance) calculations (for clustering)
- [Likelihood](#likelihood) calculations (for Bayes)
- [Report](#report)  central tendency and diversity
- [Update](#update) summarization
------------------------------------------------------------



```lua
local all = require"all"
local chat,obj,push,the = all.chat, all.obj, all.push, all.the
```


### Create

> ***[SYM](sym.md#create)(`at`:?int=0, `txt`:?str="") :[SYM](sym.md#create)***<br>
Constructor.




```lua
local SYM = obj("SYM", function(i,at,txt)
  i.at   = at or 0   -- :num  column position 
  i.txt  = txt or "" -- :str  column name 
  i.n    = 0         -- :num  items seen so far
  i.kept = {}        -- :tab  counts of symbols seen so far
  end)
```


> ***clone(`i`:[SYM](sym.md#create)) :[SYM](sym.md#create)***<br>
Return a class of the same structure.
  



```lua
function SYM.clone(i) return SYM(i.at, i.txt) end
```


### Discretize   
> ***bin(`i`:[SYM](sym.md#create), `x`:any)***<br>
Return `x` mapped to a finite range (just return x).




```lua
function SYM.bin(i,x) return x end
```


> ***merge(`i`:[SYM](sym.md#create),`j`:[SYM](sym.md#create)):[SYM](sym.md#create)***<br>
Combine two SYMs.
   



```lua
function SYM.merge(i,j,     k)
  k = i:clone()
  for _,kept in pairs{i.kept, j.kept} do
    for x,n in pairs(kept) do k:add(x,n) end end
  return k end
```


> ***merges(`i`:[SYM](sym.md#create),`t`:tab):tab***<br>
Merge a list of bins (for symbolic y-values).




```lua
function SYM.merges(i,t,...) return t end
```


### Distance
> ***dist(`i`:[SYM](sym.md#create), `x`:any,`y`:any) :num***<br>
Return distance 0..1 between `x,y`.

Assume max distance for missing values.



```lua
function SYM.dist(i,x,y)
  return  (x=="?" or y=="?")  and 1 or x==y and 0 or 1 end
```


### Likelihood  
> ***like(`i`:[SYM](sym.md#create),`x`:any,`prior`:num) :num***<br>
Return how much `x` might belong to `i`.




```lua
function SYM.like(i,x,prior)
   return ((i.kept[x] or 0)+the.m*prior) / (i.n+the.m) end
```


### Report
 > ***div(`i`:[SYM](sym.md#create)):tab***<br>
Return `div`ersity of a column.

FYI, diversity is the  tendency _not_ to be at the central tendency.



```lua
function SYM.div(i,p)
  local ent, fun = 0, function(p) return -p*math.log(p,2) end
  for x,n in pairs(i.kept) do if n > 0 then ent=ent + fun(n/i.n) end end
  return ent end
```


> ***mid(`i`:[SYM](sym.md#create)):tab***<br>
Return a columns' `mid`ddle (central tendency).




```lua
function SYM.mid(i,p)
  local max,mode=-1,nil
  for x,n in pairs(i.kept) do if n > most then most,mode = n,x end end
  return mode end
```


### Update
> ***add(`i`:`[SYM](sym.md#create)`: `x`:any, `n`:?int=1)***<br>
Add `n` count to `i.kept[n]`.




```lua
function SYM.add(i,x,n)
  if x ~= "?" then 
    n = n or 1
    i.n = i.n+n
    i.kept[x] = n  + (i.kept[x] or 0) end end
```


That's all folks.



```lua
return SYM
```


