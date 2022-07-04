<span id="forkongithub"><a href="https://github.com/timm/shortr#shortrlua--less-but-better-xai-eyes">Fork me on GitHub</a></span>
<img align=left width=250   src="bat2.png">
LUA is a "batteries-not-included" language.
But LUA makes it easy to add in the  missing bits.
E.g. here are some "b(Ai)tteries" for XAI.

(c) 2022, Tim Menzies<br><timm@ieee.org>

**config:** [all](all.html)

**build:** [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile)  
(just used for the doco)

**demos:** [go](go.html)

**apps:** [nb](nb.html)

**functions:** [lib](lib.html)

**klasses:** [bin](bin.html)
:: [cols](cols.html)
:: [num](num.html)
:: [row](row.html)
:: [rows](rows.html)
:: [some](some.html)
:: [sym](sym.html)

<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg"></a>


num.lua
## Summarize numbers


<details><summary></summary>

```lua
local all = require"all"
local big,obj,per,push,the = all.big,all.obj,all.per,all.push,all.the
local SOME = require"some"

--> NUM(at:?int, txt:?str) :NUM -> Summarize a stream of numbers.
local NUM = obj("NUM", function(i,at,txt) 
  i.at, i.txt, i.n, i.kept =  at or 0, txt or "", 0, SOME(the.Some)
  i.w = i.txt:find"-$" and -1 or 1 end)

--> add(i:NUM: x:num, n:?int=1) -> `n` times,update `i`'s SOME object.
function NUM.add(i,x,n)
  if x ~="? " then 
   for _ = 1,(n or 1) do i.n=i.n+1; i.kept:add(x) end end end

--> bin(i:NUM: x:any) -> return `x` mapped to a finite range
function NUM.bin(i,x)
  local a = i.some:has()
  local b = (a[#a] - a[1])/the.bins
  return a[#a]==a[1] and 1 or math.floor(x/b+.5)*b end

--> clone(i:(SYM|NUM)) :(SYM|NUM) -> Return a class of the same structure.
function NUM.clone(i) return NUM(i.at, i.txt) end

--> div(i:NUM) :tab -> Return `div`ersity of a column
```

</details>


(its tendency _not_ to be a its central tendency). To understand this code
recall &pm;1 to &pm;2 sds covers 66 to 95% of the Gaussian prob. In between,
at &pm;1.28, we cover 90%. So (p90-p10)/(2*1.28) returns one sd.


<details><summary></summary>

```lua
function NUM.div(i) 
  local a=i.kept:has(); return (per(a,.9) - per(a,.1))/2.56 end

--> like(i:NUM, x:any) -> Return the likelihood that `x` belongs to `i`.
function NUM.like(i,x,...)
  local sd,mu=i:div(), i:mid()
  if sd==0 then return x==mu and 1 or 1/big end
  return math.exp(-.5*((x - mu)/sd)^2) / (sd*((2*math.pi)^0.5)) end  

--> merge(i:NUM,j:NUM) :NUM -> combine two numerics
function NUM.merge(i,j,     k)
  k = i:clone()
  for _,kept in pairs{i.kept, j.kept} do
    for _,x in pairs(kept) do k:add(x) end end
  return k end

--> merge(i:NUM,t:[BIN]) :[BIN] -> merge a list of bins (for numeric y-values)
```

</details>


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

--> mid(i:NUM)) :tab -> Return a columns' `mid`ddle
function NUM.mid(i) 
  local a=i.kept:has(); return per(a,.5) end

--> norm(i:NUM, x:num) :num -> Normalize `x` 0..1 for lo..hi,
function NUM.norm(i,x)
  local a=i.kept:has(); return (a[#a]-a[1])<1E-9 or (x-a[1])/(a[#a]-a[1]) end

return NUM
```

</details>


