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


some.lua
## Keep some nums


<details><summary></summary>

```lua
local all=require"all"
local obj,push,R,sort,the= all.obj, all.push, all.R, all.sort, all.the

--> SOME(max:?int) :SOME -> collect, at most, `max` numbers.
local SOME = obj("SOME", function(i,max) 
  i.kept, i.ok, i.max, i.n = {}, true, max, 0  end)

--> add(i:SOME: x:num)-> `n` times,update `i`.
```

</details>


Helper function for NUM. If full then at odds `i.some/i.x`, keep `x`
(replacing some older item, at random).


<details><summary></summary>

```lua
function SOME.add(i,x)
  if x ~= "?" then 
    i.n = i.n + 1
    if #i.kept < i.max     then i.ok=false; push(i.kept,x) 
    elseif R() < i.max/i.n then i.ok=false; i.kept[R(#i.kept)]=x end end end 

--> has(i:SOME):tab -> Ensure contents are sorted. Return those contents.
function SOME.has(i)
  i.kept = i.ok and i.kept or sort(i.kept); i.ok=true; return i.kept ; end

return SOME
```

</details>


