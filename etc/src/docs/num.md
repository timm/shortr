<a href="all.md"><img align=left width=400 src="stark.jpeg"></a>

# [:high_brightness: SHORTr : less (but better) XAI](all.md)


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


<br clear=all>&nbsp;
<p align=center>
<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
</p>

## Class NUM
Summarize numbers

**RESPONSIBILITIES** : 
- [Create](#create) a duplicate structure 
- [Discretize](#discretize) numerics into a few bins (for building trees)
- [Distance](#distance) calculations (for clustering)
- [Likelihood](#likelihood) calculations (for Bayes)
- [Report](#report)  central tendency and diversity
- [Update](#update) summarization
- Knows if we want to minimize or maximize these values (see `w`).

**COLLABORATORS** :
- [SOME](some.md) : used to store a sample of the items seen oo far.
------------------------------------------------------------



```lua
local all = require"all"
local big,obj,per,push,the = all.big,all.obj,all.per,all.push,all.the
local SOME = require"some"
```


### Create
> ***[NUM](num.md#create)(`at`:?int, `txt`:?str) :[NUM](num.md#create)***<br>
Summarize a stream of numbers.




```lua
local NUM = obj("NUM", function(i,at,txt) 
  i.at   = at or 0                 -- :num   column position 
  i.txt  = txt or ""               -- :str   column name 
  i.n    = 0                       -- :num   items seen so far
  i.kept = SOME(the.Some)          -- :SOME  holds a sample of items seen so far
  i.w = i.txt:find"-$" and -1 or 1 -- :num   do we seek less or more of this?
  end)
```


> ***clone(`i`:[NUM](num.md#create)) :[NUM](num.md#create)***<br>
Return a class of the same structure.




```lua
function NUM.clone(i) return NUM(i.at, i.txt) end
```


### Discretize
> ***bin(`i`:`[NUM](num.md#create)`: `x`:any)***<br>
Return `x` mapped to a finite range




```lua
function NUM.bin(i,x)
  local a = i.kept:has()
  local b = (a[#a] - a[1])/the.bins
  return a[#a]==a[1] and 1 or math.floor(x/b+.5)*b end
```


> ***merge(`i`:[NUM](num.md#create),`j`:[NUM](num.md#create)) :[NUM](num.md#create)***<br>
combine two numerics




```lua
function NUM.merge(i,j,     k)
  k = i:clone()
  for _,kept in pairs{i.kept, j.kept} do
    for _,x in pairs(kept) do k:add(x) end end
  return k end
```


> ***merges(`i`:[NUM](num.md#create),`t`:[[BIN](bin.md#create)]) :[[BIN](bin.md#create)]***<br>
merge a list of bins (for numeric y-values)

Note the last kine of `merges`: if anything merged, then loop again looking for other merges.
Also, at the end, expand bins to cover all gaps across the number line.
Finally, to see what happens when this code calls `merged`, goto [BIN](bin.md).



```lua
function NUM.merges(i,b4, min) 
  local function fillInTheGaps(bins)
    bins[1].lo, bins[#bins].hi = -big, big
    if #bins>1 then
      for n=2,#bins do bins[n].lo = bins[n-1].hi end end
    return bins 
  end ------------- 
  local n,now = 1,{}
  while n <= #b4 do
    local merged = n<#b4 and b4[n]:merged(b4[n+1], min)
    now[#now+1]  = merged or b4[n]
    n            = n + (merged and 2 or 1)  end
  return #now < #b4 and i:merges(now,min) or fillInTheGaps(now) end
```


### Distance
> ***dist(`i`:[NUM](num.md#create), `x`:num,`y`:num): num***<br>
Return distance 0..1 between `x,y`.

This code assume max distance for missing values.



```lua
function NUM.dist(i,x,y)
  if x=="?" and y=="?" then return 1 end
  if     x=="?" then y = i:norm(y); x = y<.5 and 1 or 0 
  elseif y=="?" then x = i:norm(x); y = x<.5 and 1 or 0
  else   x,y = i:norm(x), i:norm(y) end
  return math.abs(x - y) end 
```


### Likelihood
> ***like(`i`:[NUM](num.md#create), `x`:any)***<br>
Return the likelihood that `x` belongs to `i`.




```lua
function NUM.like(i,x,...)
  local sd,mu=i:div(), i:mid()
  if sd==0 then return x==mu and 1 or 1/big end
  return math.exp(-.5*((x - mu)/sd)^2) / (sd*((2*math.pi)^0.5)) end  
```


### Report
> ***div(`i`:[NUM](num.md#create)) :tab***<br>
Return `div`ersity of a column (tendency to depart central tendency).

To understand `div`  recall &pm;1 to &pm;2 sds covers 66 to 95% of the Gaussian prob.
In between, at &pm;1.28, we cover 90%. So (p90-p10)/(2*1.28) returns one sd.



```lua
function NUM.div(i) 
  local a=i.kept:has(); return (per(a,.9) - per(a,.1))/2.56 end
```


> ***mid(`i`:[NUM](num.md#create))) :tab***<br>
Return a columns' `mid`ddle




```lua
function NUM.mid(i) 
  local a=i.kept:has(); return per(a,.5) end
```


> ***norm(`i`:[NUM](num.md#create), `x`:num) :num***<br>
Normalize `x` 0..1 for lo..hi




```lua
function NUM.norm(i,x)
  local a=i.kept:has(); return (a[#a]-a[1])<1E-9 or (x-a[1])/(a[#a]-a[1]) end
```


### Update
> ***add(`i`:[NUM](num.md#create), `x`:num, `n`:?int=1)***<br>
`n` times,update `i`'s SOME object.




```lua
function NUM.add(i,x,n)
  if x ~="? " then 
   for _ = 1,(n or 1) do i.n=i.n+1; i.kept:add(x) end end end
```


That's all folks.



```lua
return NUM
```


