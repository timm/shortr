# B(Ai)ttery

<img align=left width=400 src="bat2.png">

LUA is a "batteries-not-included" language.   
But LUA makes it easy to add in the  missing bits.   
E.g. here are some "b(Ai)tteries" for XAI (explainable artifical intelligence).   
:flashlight:
:electric_plug:
:battery:

(c) 2022, Tim Menzies <timm@ieee.org>

|what          | where |
|-------------:|:------|
|**config**    | [all](all.md)   |
|**build**     | [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile) (just for doco)  | 
|**demos**     | [go](go.md)  |
|**apps**      | [nb](nb.md)  |
|**functions** | [lib](lib.md) |  
|**methods**   | [bin](bin.md), [cols](cols.md), [num](num.md), [row](row.md), [rows](rows.md), [some](some.md), [sym](sym.md) |

<br clear=all>
<p align=center>
<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/Linux-FCC624?logo=linux&logoColor=black"></a>
<a href=".."><img src="https://img.shields.io/badge/mac%20os-000000?logo=apple&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/VIM-%2311AB00.svg?logo=vim&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
</p>


## Summarize numbers


<details><summary></summary>

```lua
local all = require"all"
local big,obj,per,push,the = all.big,all.obj,all.per,all.push,all.the
local SOME = require"some"
```

</details>


> ***NUM(`at` :?int, `txt` :?str) :NUM***<br>  Summarize a stream of numbers.


<details><summary></summary>

```lua
local NUM = obj("NUM", function(i,at,txt) 
  i.at, i.txt, i.n, i.kept =  at or 0, txt or "", 0, SOME(the.Some)
  i.w = i.txt:find"-$" and -1 or 1 end)
```

</details>


> ***add(`i` :`NUM` : `x` :num, `n` :?int=1)***<br>  `n` times,update `i`'s SOME object.


<details><summary></summary>

```lua
function NUM.add(i,x,n)
  if x ~="? " then 
   for _ = 1,(n or 1) do i.n=i.n+1; i.kept:add(x) end end end
```

</details>


> ***bin(`i` :`NUM` : `x` :any)***<br>  return `x` mapped to a finite range


<details><summary></summary>

```lua
function NUM.bin(i,x)
  local a = i.some:has()
  local b = (a[#a] - a[1])/the.bins
  return a[#a]==a[1] and 1 or math.floor(x/b+.5)*b end
```

</details>


> ***clone(`i` :(SYM|NUM)) :(SYM|NUM)***<br>  Return a class of the same structure.


<details><summary></summary>

```lua
function NUM.clone(i) return NUM(i.at, i.txt) end
```

</details>


> ***div(`i` :NUM) :tab***<br>  Return `div`ersity of a column
(its tendency _not_ to be a its central tendency). To understand this code
recall &pm;1 to &pm;2 sds covers 66 to 95% of the Gaussian prob. In between,
at &pm;1.28, we cover 90%. So (p90-p10)/(2*1.28) returns one sd.


<details><summary></summary>

```lua
function NUM.div(i) 
  local a=i.kept:has(); return (per(a,.9) - per(a,.1))/2.56 end
```

</details>


> ***like(`i` :NUM, `x` :any)***<br>  Return the likelihood that `x` belongs to `i`.


<details><summary></summary>

```lua
function NUM.like(i,x,...)
  local sd,mu=i:div(), i:mid()
  if sd==0 then return x==mu and 1 or 1/big end
  return math.exp(-.5*((x - mu)/sd)^2) / (sd*((2*math.pi)^0.5)) end  
```

</details>


> ***merge(`i` :NUM,`j` :NUM) :NUM***<br>  combine two numerics


<details><summary></summary>

```lua
function NUM.merge(i,j,     k)
  k = i:clone()
  for _,kept in pairs{i.kept, j.kept} do
    for _,x in pairs(kept) do k:add(x) end end
  return k end
```

</details>


> ***merge(`i` :NUM,`t` :[BIN]) :[BIN]***<br>  merge a list of bins (for numeric y-values)
If anything merged, then loop again looking for other merges.
At the end, expand bins to cover all gaps across the number line.


<details><summary></summary>

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

</details>


> ***mid(`i` :NUM)) :tab***<br>  Return a columns' `mid`ddle


<details><summary></summary>

```lua
function NUM.mid(i) 
  local a=i.kept:has(); return per(a,.5) end
```

</details>


> ***norm(`i` :NUM, `x` :num) :num***<br>  Normalize `x` 0..1 for lo..hi,


<details><summary></summary>

```lua
function NUM.norm(i,x)
  local a=i.kept:has(); return (a[#a]-a[1])<1E-9 or (x-a[1])/(a[#a]-a[1]) end

return NUM
```

</details>


