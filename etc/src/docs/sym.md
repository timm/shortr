<a href="all.md"><img align=right width=350 src="bat2.png"></a>

# [:high_brightness: B(Ai)ttery](all.md)


LUA is a "batteries-not-included" language.   But LUA makes it easy to add in the missing bits.
E.g. here are some "b(Ai)tteries" for XAI (explainable artifical intelligence).   

|what      | where |
|---------:|:------|
|config    | [all](all.md)   |
|build     | [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile) (just for doco)  | 
|demos     | [go](go.md)  |
|apps      | [nb](nb.md), [tree](tree.md)  |
|functions | [lib](lib.md) |  
|methods   | [bin](bin.md), [cols](cols.md), [num](num.md), [row](row.md), [rows](rows.md), [some](some.md), [sym](sym.md) |

<br clear=all>
<p align=center>
<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
</p>

## Summarize symbols



```lua
local all = require"all"
local chat,obj,push,the = all.chat, all.obj, all.push, all.the
```


> ***SYM(`at` :?int, `txt` :?str) :SYM***&nbsp;  &nbsp;  &nbsp;   &nbsp;  &nbsp;  &nbsp; &nbsp;  &nbsp;  &nbsp; :speech_balloon:  Summarize a stream of non-numerics.  



```lua
local SYM = obj("SYM", function(i,at,txt)
  i.at, i.txt, i.n, i.kept =  at or 0, txt or "", 0, {} end)
```


> ***add(`i` :`SYM` : `x` :any, `n` :?int=1)***&nbsp;  &nbsp;  &nbsp;   &nbsp;  &nbsp;  &nbsp; &nbsp;  &nbsp;  &nbsp; :speech_balloon:  Add `n` count to `i.kept[n]` .  



```lua
function SYM.add(i,x,n)
  if x ~= "?" then 
    i.n = i.n+1
    i.kept[x] = (n or 1) + (i.kept[x] or 0) end end
```


> ***bin(`i` :`SYM` : `x` :any)***&nbsp;  &nbsp;  &nbsp;   &nbsp;  &nbsp;  &nbsp; &nbsp;  &nbsp;  &nbsp; :speech_balloon:  return `x` mapped to a finite range (just return x)  



```lua
function SYM.bin(x) return x end
```


> ***clone(`i` :SYM) :SYM***&nbsp;  &nbsp;  &nbsp;   &nbsp;  &nbsp;  &nbsp; &nbsp;  &nbsp;  &nbsp; :speech_balloon:  Return a class of the same structure.  



```lua
function SYM.clone(i) return SYM(i.at, i.txt) end
```


> ***like(`i` :SYN,`x` :any,`prior` :num):num***&nbsp;  &nbsp;  &nbsp;   &nbsp;  &nbsp;  &nbsp; &nbsp;  &nbsp;  &nbsp; :speech_balloon:  return how much `x` might belong to `i`.  



```lua
function SYM.like(i,x,prior)
   return ((i.kept[x] or 0)+the.m*prior) / (i.n+the.m) end
```


> ***merge(`i` :SYM,`j` :SYM):SYM***&nbsp;  &nbsp;  &nbsp;   &nbsp;  &nbsp;  &nbsp; &nbsp;  &nbsp;  &nbsp; :speech_balloon:  combine two symbols  



```lua
function SYM.merge(i,j,     k)
  k = i:clone()
  for _,kept in pairs{i.kept, j.kept} do
    for x,n in pairs(kept) do k:add(x,n) end end
  return k end
```


> ***merge(`i` :SYM,`t` :tab):tab***&nbsp;  &nbsp;  &nbsp;   &nbsp;  &nbsp;  &nbsp; &nbsp;  &nbsp;  &nbsp; :speech_balloon:  merge a list of bins (for symbolic y-values)  



```lua
function SYM.merges(i,t,...) return t end
```


> ***mid(`i` :SYM):tab***&nbsp;  &nbsp;  &nbsp;   &nbsp;  &nbsp;  &nbsp; &nbsp;  &nbsp;  &nbsp; :speech_balloon:  Return a columns' `mid`ddle (central tendency).  



```lua
function SYM.mid(i,p)
  local max,mode=-1,nil
  for x,n in pairs(i.kept) do if n > most then most,mode = n,x end end
  return mode end
```


> ***div(`i` :SYM):tab***&nbsp;  &nbsp;  &nbsp;   &nbsp;  &nbsp;  &nbsp; &nbsp;  &nbsp;  &nbsp; :speech_balloon:  Return `div`ersity of a column (its tendency _not_ to be a its central tendency).  



```lua
function SYM.div(i,p)
  local ent, fun = 0, function(p) return -p*math.log(p,2) end
  for x,n in pairs(i.kept) do if n > 0 then ent=ent + fun(n/i.n) end end
  return ent end
```


That's all folks.



```lua
return SYM
```


