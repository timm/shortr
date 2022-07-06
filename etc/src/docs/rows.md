# [:high_brightness: SHORTr : less (but better) XAI](all.md)

<!-- a href="all.md"><img align=right width=500 src="https://ernesto.net/wp-content/uploads/2021/01/img6-home5.png"></a --->
<a href="all.md"><img align=right width=500 src="xai4.png"></a>

AI and XAI (explainable artificial intelligence) need not be complicated.
For example, here we need just a few 100 lines of LUA to search
N items to  find and explain the best ones, using just log(N) evals. Along the way,
the object model we build could also be applied to  many other AI tasks (nearest neighbor,
decision trees, bayes classifiers, etc).



|       what | where                                                                                                                                                                                     |
|-----------:|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| start here | [help](all.md) &hellip;  [install](/INSTALL.md) &hellip; [design notes](design.md)                                                                                                        |
|      build | [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile) (just for doco)                                                                                                   |
|      demos | [go](go.md)                                                                                                                                                                               |
|       apps | [nb](nb.md) &hellip; [tree](tree.md)                                                                                                                                                      |
|  functions | [lib](lib.md)                                                                                                                                                                             |
|    methods | [bin](bin.md) &hellip; [cols](cols.md) &hellip; [num](num.md) &hellip; [row](row.md)<br> [rows](rows.md) &hellip; [some](some.md) &hellip; [sym](sym.md) &hellip; [tree](tree.md) |

<br clear=all>
<p align=center>
<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
</p>

## Class ROWS
 ROWS stores (and makes summaries  of )    ROWs.

**RESPONSIBILITIES** : 
- Store many ROWs
- Load csv files into ROWS (see `fill`)
- Summarize ROWs in NUM or SYM columns (see `add`)
- Report summaries (see `mid`)
- Clone (create new ROWS with the same structure) (see `clone`)
- Distance calculations (see `dist`)
- Bayesian likelihood calculations (see `add`)   

**COLLABORATORS** :
- ROW, COLS (and COLS are factories  that decide what NUMs or SYMs to make).
------------------------------------------------------------



```lua
local all = require"all"
local chat,csv,map,obj  = all.chat, all.csv, all.map,  all.obj
local push,rnd,rnds,the = all.push, all.rnd, all.rnds, all.the
local COLS,ROW          = require"COLS",require"ROW"
```


ROWS(names:?[str], rows:?[ROW}) :ROWS --> Place to store many ROWS
 and summarize them (in `i.cols`).



```lua
local ROWS = obj("ROWS", function(i,names,rows) 
  i.rows, i.cols = {}, (names and COLS(names) or nil)
  for _,row in pairs(rows or {}) do i:add(row) end end)
```


add(i:ROWS: row:ROW) --> add ROW to ROWS, update the summaries in `i.cols`.



```lua
function ROWS.add(i,t) 
  t = t.cells and t or ROW(i,t)
  if i.cols then i.cols:add(push(i.rows, t)) else i.cols=COLS(t.cells) end 
  return t end
```


ROWS.clone(init:?[ROW]) :ROWS --> Return a ROWS with same structure as `i`. 
Optionally, `init`ialize it with some rows. Add a pointer back to the 
original table that spawned `eve`rything else (useful for some distance calcs).



```lua
function ROWS.clone(i,init)
  local j=ROWS(i.cols.names,init)
  return j end
```


fill(i:ROWS: src:(str|tab)):ROWS --> copy the data from `src` into `i`.



```lua
function ROWS.fill(i,src)
  local iterate = type(src)=="table" and map or csv
  iterate(src, function(t) i:add(t) end) 
  return i end
```


like(i:ROWS,row;ROW,nklasses:num,nrows:num):num --> ***Return -- P(H)*&prod;<sub***<br>
i
/sub> ***(P(E<sub***<br>
i
/sub>|H)). 
Do it with logs to handle very small numbers.



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
```


mids(i:ROW,p:?int=2,cols=?[COL]=i.cols.y):tab --> Return `mid` of columns rounded to `p` places.



```lua
function ROWS.mids(i,p,cols) 
  local t={n=#i.rows}
  for _,col in pairs(cols or i.cols.y) do t[col.txt]=col:mid(p) end
  return rnds(t,p or 2) end
```


That's all folks.



```lua
return ROWS
```


