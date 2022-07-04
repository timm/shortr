# B(Ai)ttery

<img align=left width=400 src="bat2.png">

LUA is a "batteries-not-included" language.   
But LUA makes it easy to add in the  missing bits.   
E.g. here are some "b(Ai)tteries" for XAI (explainable artifical intelligence)

(c) 2022, Tim Menzies <timm@ieee.org>

|what          | where |
|:-------------|:------|
|**config**    | [all](all.md)   |
|**build**     | [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile) (just for doco)  | 
|**demos**     | [go](go.md)  |
|**apps**      | [nb](nb.md)  |
|**functions** | [lib](lib.md) |  
|**methods**   | [bin](bin.md) :: [cols](cols.md) :: [num](num.md) :: [row](row.md) :: [rows](rows.md) :: [some](some.md) :: [sym](sym.md) |

<br clear=all>
<p align=center>
<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/Linux-FCC624?logo=linux&logoColor=black"></a>
<a href=".."><img src="https://img.shields.io/badge/mac%20os-000000?logo=apple&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/VIM-%2311AB00.svg?logo=vim&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative.svg"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
</p>


## Lua to Narkdiwn


<details><summary></summary>

```lua
local all=require"all"
local chunkscat,lines,push = all.cat, all.lines, all.push
```

</details>


asdas
asdasaa  asdas
adasa s

asda


<details><summary></summary>

```lua
local doc={}
```

</details>


> ***chunks(`x` :int, `y` :[fred]) :int***<br>  asdads


<details><summary></summary>

```lua
function doc.chunks(file)
  local prep=function(t) 
    if t[#t]:find"^[%s]*$" then t[#t]=nil end; return table.concat(t,"\n") end
  local b4,now,t,out=0, 0,{},{}
  lines(file, function(s)
    now=b4
    if s:sub(1,3)=="-- " then now=0; s=s:sub(4) elseif s:find"^%S" then now=1 end
    if now==b4 then push(t,s) else push(out, {what=now, txt=prep(t)}) ; t={s} end
    b4 = now end)
  if #t>0 then push(out,{what=now, txt=prep(t)}) end
  return out end

for n,chunk in pairs(doc.chunks("doc.lua")) do print(""); print(n,chunk.what,"[["..chunk.txt.."]]") end

asdas=2
```

</details>


asdas
saas


<details><summary></summary>

```lua
asdas = asda
```

</details>


