# [:high_brightness: B(Ai)ttery](all.md)

<a href="all.md"><img align=right width=350 src="bat2.png"></a>

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

##  How to make NUMs or SYMs



```lua
local all=require"all"
local obj, push = all.obj, all.push
local NUM, SYM = require"NUM", require"SYM"

--> COLS(names:[str]) :COLS -> Factory. Turns a list of names into NUMs or SYMs.
```


Goal columns get added to `i.y` and others to `i.x` (unless denoted `ignored`). 
A klass column goes to `i.klass`.



```lua
local COLS = obj("COLS", function(i,names) 
  i.names, i.x, i.y, i.all,i.klass, i.names = names, {}, {},  {}
  for at,txt in pairs(names) do
    local col = (txt:find"^[A-Z]" and NUM or SYM)(at,txt) 
    push(i.all, col)
    if not col.txt:find":$" then
      push(col.txt:find"[!+-]$" and i.y or i.x, col)
      if col.txt:find"!$" then i.klass=col end end end end ) 

--> add(i:COLS: row:ROW) -> Update columns using data from `row`.
function COLS.add(i,row)
  for _,cols in pairs{i.x,i.y} do
    for _,col in pairs(cols) do col:add(row.cells[col.at]) end end end

return COLS
```


