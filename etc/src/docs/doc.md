<a name=top></a> 

# :high_brightness: B(Ai)ttery

<img align=left width=400 src="bat2.png">

LUA is a "batteries-not-included" language.   
But LUA makes it easy to add in the  missing bits.   
E.g. here are some "b(Ai)tteries" for XAI (explainable artifical intelligence).   

(c) 2022, Tim Menzies <timm@ieee.org>

|what          | where |
|-------------:|:------|
|**config**    | [all](all.md#top)   |
|**build**     | [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile) (just for doco)  | 
|**demos**     | [go](https://github.com/timm/shortr/blob/master/etc/src/go.md#top)  |
|**apps**      | [nb](nb.md#top), [tree](tree.md#top)  |
|**functions** | [lib](lib.md#top) |  
|**methods**   | [bin](bin.md#top), [cols](cols.md#top), [num](num.md#top), [row](row.md#top), [rows](rows.md#top), [some](some.md#top), [sym](sym.md#top) |

In this code,  `ROW` holds one record while `ROWS` holds lots of `ROWs`. Each `ROWS` summarizes numeric
or symbolic  columns in `NUM`s or `SYM`s, respectively. These summaries are held in `COLS` which divide the columns into (x,y) sets (for
independent and dependent columns, respectively). Pairs of (x,y) columns are summarized in `BIN`s (and adjacent `BIN`s that have similar y distributions
are merged). 
`SOME` is a helper
class for `NUM`s that retains some sample of all the numerics in that column. 

Everything else is just tiny extensions to the above object model. e.g. 
decision `TREE`s are built by recursively finding the `BIN`s that best distinguish different `ROW`s. 

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


## Lua to Narkdiwn



```lua
local all=require"all"
local chunkscat,lines,push = all.cat, all.lines, all.push
```


asdas
asdasaa  asdas
adasa s

asda



```lua
local doc={}
```


> ***chunks(`x` :int, `y` :[fred]) :int***<br>  asdads



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


asdas
saas



```lua
asdas = asda
```


