# [:high_brightness: B(Ai)ttery](all.md)

<a href="all.md"><img align=left width=400 src="bat2.png"></a>

LUA is a "batteries-not-included" language.   
But LUA makes it easy to add in the  missing bits.   
E.g. here are some "b(Ai)tteries" for XAI (explainable artifical intelligence).   

(c) 2022, Tim Menzies <timm@ieee.org>

|what          | where |
|-------------:|:------|
|**config**    | [all](all.md)   |
|**build**     | [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile) (just for doco)  | 
|**demos**     | [go](go.md)  |
|**apps**      | [nb](nb.md), [tree](tree.md)  |
|**functions** | [lib](lib.md) |  
|**methods**   | [bin](bin.md), [cols](cols.md), [num](num.md), [row](row.md), [rows](rows.md), [some](some.md), [sym](sym.md) |

In this code :
-   `ROWS` holds many `ROW`s (and each `ROW` holds one record).  `ROWS` summarize their numeric
or symbolic  columns in `NUM`s or `SYM`s (respectively). Summaries are held in `COLS`, divided into  (x,y) sets for
independent and dependent columns (respectively). 
- `BIN`s and `SOME` are helper classes. Pairs of (x,y) columns are summarized in `BIN`s. Adjacent `BIN`s with  similar y distributions
are merged. 
`SOME` is a helper
for `NUM`s that holds just some sample of the numerics in that column. 
- Everything else is just tiny extensions to the above object model. e.g. 
  - When clustering, each cluster is its own `ROWS`.
  - `NB` classifiers create one `ROWS` per class in the training data.
  - Decision `TREE`s are built by recursively finding the `BIN`s that best distinguish different `ROW`s. 
  - etc.

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


## store many ROWs



```lua
local all = require"all"
local chat,csv,map,obj  = all.chat, all.csv, all.map,  all.obj
local push,rnd,rnds,the = all.push, all.rnd, all.rnds, all.the
local COLS,ROW          = require"COLS",require"ROW"

--> ROWS(names:?[str], rows:?[ROW}) :ROWS -> Place to store many ROWS
```


 and summarize them (in `i.cols`).



```lua
local ROWS = obj("ROWS", function(i,names,rows) 
  i.rows, i.cols = {}, (names and COLS(names) or nil)
  for _,row in pairs(rows or {}) do i:add(row) end end)

--> add(i:ROWS: row:ROW) -> add ROW to ROWS, update the summaries in `i.cols`.
function ROWS.add(i,t) 
  t = t.cells and t or ROW(i,t)
  if i.cols then i.cols:add(push(i.rows, t)) else i.cols=COLS(t.cells) end 
  return t end

--> ROWS.clone(init:?[ROW]) :ROWS -> Return a ROWS with same structure as `i`. 
```


Optionally, `init`ialize it with some rows. Add a pointer back to the 
original table that spawned `eve`rything else (useful for some distance calcs).



```lua
function ROWS.clone(i,init)
  local j=ROWS(i.cols.names,init)
  return j end

--> fill(i:ROWS: src:(str|tab)):ROWS -> copy the data from `src` into `i`.
function ROWS.fill(i,src)
  local iterate = type(src)=="table" and map or csv
  iterate(src, function(t) i:add(t) end) 
  return i end

--> like(i:ROWS,row;ROW,nklasses:num,nrows:num):num -> Return 
```


P(H)*&prod;<sub>i</sub> (P(E<sub>i</sub>|H)). Do it with logs
to handle very small numbers.



```lua
function ROWS.like(i,row, nklasses, nrows)
  local prior,like,inc,x
  prior = (#i.rows + the.k) / (nrows + the.k * nklasses)
  like  = math.log(prior)
  row = row.cells and row.cells or row
  for _,col in pairs(i.cols.x) do
    x = row[col.at]
    if x ~= nil and x ~= "?" then
      inc  = col:like(x,prior)
      like = like + math.log(inc) end end
  return like end

--> mids(i:ROW,p:?int=2,cols=?[COL]=i.cols.y):tab -> Return `mid` of columnss
```


rounded to `p` places.



```lua
function ROWS.mids(i,p,cols) 
  local t={}
  for _,col in pairs(cols or i.cols.y) do t[col.txt]=col:mid(p) end
  return rnds(t,p or 2) end

return ROWS
```


