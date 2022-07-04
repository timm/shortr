<img align=left width=250   src="bat2.png">

# B(Ai)ttery
LUA is a "batteries-not-included" language.
But LUA makes it easy to add in the  missing bits.
E.g. here are some "b(Ai)tteries" for XAI.

(c) 2022, Tim Menzies<br><timm@ieee.org>
<br clear=all>

**config:** 
**build:** 
**demos:** 
**apps:** 
**functions:** 
**klasses:** 

|config | build | demos | apps | functions | classes |
|-------|-------|-------|------|-----------|---------|
|[all](all.html)|[Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile)<br>(just for doco)|[go](go.html)|[nb](nb.html)|[lib](lib.html)|[bin](bin.html) :: [cols](cols.html) :: [num](num.html) :: [row](row.html) :: [rows](rows.html) :: [some](some.html) :: [sym](sym.html)|

<a href=".."><img src="https://img.shields.io/badge/Language--lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 


sym.lua
## Summarize symbols


<details><summary></summary>

```lua
local all = require"all"
local chat,obj,push,the = all.chat, all.obj, all.push, all.the
--> SYM(at:?int, txt:?str) :SYM -> Summarize a stream of non-numerics.
local SYM = obj("SYM", function(i,at,txt)
  i.at, i.txt, i.n, i.kept =  at or 0, txt or "", 0, {} end)

--> add(i:SYM: x:any, n:?int=1) -> Add `n` count to `i.kept[n]` .
function SYM.add(i,x,n)
  if x ~= "?" then 
    i.n = i.n+1
    i.kept[x] = (n or 1) + (i.kept[x] or 0) end end

--> bin(i:SYM: x:any) -> return `x` mapped to a finite range (just return x)
function SYM.bin(x) return x end

--> clone(i:SYM) :SYM -> Return a class of the same structure.
function SYM.clone(i) return SYM(i.at, i.txt) end

--> like(i:SYN,x:any,prior:num):num -> return how much `x` might belong to `i`.
function SYM.like(i,x,prior)
   return ((i.kept[x] or 0)+the.m*prior) / (i.n+the.m) end

--> merge(i:SYM,j:SYM):SYM -> combine two symbols
function SYM.merge(i,j,     k)
  k = i:clone()
  for _,kept in pairs{i.kept, j.kept} do
    for x,n in pairs(kept) do k:add(x,n) end end
  return k end

--> merge(i:SYM,t:tab):tab -> merge a list of bins (for symbolic y-values)
function SYM.merges(i,t,...) return t end
 
--> mid(i:SYM):tab -> Return a columns' `mid`ddle (central tendency).
function SYM.mid(i,p)
  local max,mode=-1,nil
  for x,n in pairs(i.kept) do if n > most then most,mode = n,x end end
  return mode end

--> div(i:SYM):tab -> Return `div`ersity of a column
```

</details>


(its tendency _not_ to be a its central tendency).


<details><summary></summary>

```lua
function SYM.div(i,p)
  local ent, fun = 0, function(p) return -p*math.log(p,2) end
  for x,n in pairs(i.kept) do if n > 0 then ent=ent + fun(n/i.n) end end
  return ent end
 
return SYM
```

</details>


