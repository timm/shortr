## Class SYM
Summarize symbols

**RESPONSIBILITIES** : 
- [Create](#create) a duplicate structure 
- [Discretize](#discretize) numerics into a few bins (for building trees)
- [Distance](#distance) calculations (for clustering)
- [Likelihood](#likelihood) calculations (for Bayes)
- [Report](#report)  central tendency and diversity
- [Update](#update) summarization


| What                                                   | Notes                                                |
|--------------------------------------------------------|------------------------------------------------------|
| [***SYM(`at` :?int=0, `txt` :?str="")  :SYM***](#1)    | Constructor.                                         |
| [***clone(`i` :SYM)  :SYM***](#2)                      | Return a class of the same structure.                |
| [***bin(`i` :SYM, `x` :any)***](#3)                    | Return `x` mapped to a finite range (just return x). |
| [***merge(`i` :SYM,`j` :SYM) :SYM***](#4)              | Combine two SYMs.                                    |
| [***merges(`i` :SYM,`t` :tab) :tab***](#5)             | Merge a list of bins (for symbolic y-values).        |
| [***dist(`i` :SYM, `x` :any,`y` :any)  :num***](#6)    | Return distance 0..1 between `x,y`.                  |
| [***like(`i` :SYM,`x` :any,`prior` :num)  :num***](#7) | Return how much `x` might belong to `i`.             |
| [***mid(`i` :SYM) :tab***](#8)                         | Return a columns' `mid`ddle (central tendency).      |
| [***add(`i` :`SYM` : `x` :any, `n` :?int=1)***](#9)    | Add `n` count to `i.kept[n]`.                        |

```lua
local all = require"all"
local chat,obj,push,the = all.chat, all.obj, all.push, all.the
```

### Create

> [](#0)
***SYM(`at` :?int=0, `txt` :?str="")  :SYM***<br>
Constructor.


```lua
local SYM = obj("SYM", function(i,at,txt)
  i.at   = at or 0   -- :num  column position 
  i.txt  = txt or "" -- :str  column name 
  i.n    = 0         -- :num  items seen so far
  i.kept = {}        -- :tab  counts of symbols seen so far
  end)
```

> [](#1)
***clone(`i` :SYM)  :SYM***<br>
Return a class of the same structure.



```lua
function SYM.clone(i) return SYM(i.at, i.txt) end
```

### Discretize   
> [](#2)
***bin(`i` :SYM, `x` :any)***<br>
Return `x` mapped to a finite range (just return x).


```lua
function SYM.bin(i,x) return x end
```

> [](#3)
***merge(`i` :SYM,`j` :SYM) :SYM***<br>
Combine two SYMs.



```lua
function SYM.merge(i,j,     k)
  k = i:clone()
  for _,kept in pairs{i.kept, j.kept} do
    for x,n in pairs(kept) do k:add(x,n) end end
  return k end
```

> [](#4)
***merges(`i` :SYM,`t` :tab) :tab***<br>
Merge a list of bins (for symbolic y-values).



```lua
function SYM.merges(i,t,...) return t end
```

### Distance
> [](#5)
***dist(`i` :SYM, `x` :any,`y` :any)  :num***<br>
Return distance 0..1 between `x,y`.

Assume max distance for missing values.

```lua
function SYM.dist(i,x,y)
  return  (x=="?" or y=="?")  and 1 or x==y and 0 or 1 end
```

### Likelihood  
> [](#6)
***like(`i` :SYM,`x` :any,`prior` :num)  :num***<br>
Return how much `x` might belong to `i`.


```lua
function SYM.like(i,x,prior)
   return ((i.kept[x] or 0)+the.m*prior) / (i.n+the.m) end
```

### Report
 > div(i:SYM):tab  > Return `div`ersity of a column. <
FYI, diversity is the  tendency _not_ to be at the central tendency.

```lua
function SYM.div(i,p)
  local ent, fun = 0, function(p) return -p*math.log(p,2) end
  for x,n in pairs(i.kept) do if n > 0 then ent=ent + fun(n/i.n) end end
  return ent end
```

> [](#7)
***mid(`i` :SYM) :tab***<br>
Return a columns' `mid`ddle (central tendency).



```lua
function SYM.mid(i,p)
  local max,mode=-1,nil
  for x,n in pairs(i.kept) do if n > most then most,mode = n,x end end
  return mode end
```

### Update
> [](#8)
***add(`i` :`SYM` : `x` :any, `n` :?int=1)***<br>
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

