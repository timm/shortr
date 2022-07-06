# [:high_brightness: SHORTr : less (but better) XAI](all.md)

<!-- a href="all.md"><img align=right width=500 src="https://ernesto.net/wp-content/uploads/2021/01/img6-home5.png"></a --->

AI and XAI (explainable artificial intelligence) need not be
complicated.  For example, here we need just a few 100 lines of LUA
to search N items to  find and explain the best ones, using just
log(N) evals.  Along the way, we built an object model that could
also be applied to  many other AI tasks (nearest neighbor, decision
trees, bayes classifiers, etc).

<a href="all.md"><img align=right width=600 src="xai4.png"></a>

|       what | where                                                                                                                                                                                     |
|-----------:|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| start here | [help](all.md) &hellip;  [install](/INSTALL.md) &hellip; [design notes](design.md)                                                                                                        |
|      build | [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile) (just for doco)                                                                                                   |
|      demos | [go](go.md)                                                                                                                                                                               |
|       apps | [nb](nb.md) &hellip; [tree](tree.md)                                                                                                                                                      |
|  functions | [lib](lib.md)                                                                                                                                                                             |
|    methods | [bin](bin.md) &hellip; [cols](cols.md) &hellip; [num](num.md) &hellip; [row](row.md)<br> [rows](rows.md) &hellip; [some](some.md) &hellip; [sym](sym.md) &hellip; [tree](tree.md) |

Also, this code shows off how LUA is a really cool language
(kind of like LISP, without brackets). Given 
a dozen "ten-liners", this code supports test suites for
test-driven development, literate programming,  several tiny
domain-specific languages, object-oriented programming, command-line
arguments,  and lots of higher-order functions tricks.

<br clear=all>
<p align=center>
<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
</p>

## Class ROW
Hold one record (contained within [ROWS](rows.md)). 

**RESPONSIBILITIES** : 
- Sorting (on dependent columns) to find better ROWs (see `__lt`)
- Distance calculations (see `__sub`)
- Knows the data space that contains it (see `__of`).
- Knows its klass (see `klass`).
- Discretization (see `bin, merge, merges`)
- Distance calcs (see `dist, around, far`)

**COLLABORATORS** :
- [ROWS](rows.md) : the data space that contains it
------------------------------------------------------------



```lua
local all = require"all"
local big,chat,lt,map  = all.big, all.chat, all.lt, all.map
local obj,rnds,sort    = all.obj, all.rnds, all.sort
```


ROW(of:ROWS, cells:tab) :ROW --> Place to store one record
(and stats on how it is used; e.g. `i.evaled=true` if we touch the y values.



```lua
local ROW = obj("ROW", function(i,of,cells) 
  i.cells  = cells -- :tab  -- the stored record
  i._of    = of    -- :ROWS -- back pointer to data space that contains this
  i.evaled = false -- :bool -- true if we ever use the dependent variables.
  end)
```


better(i:ROW, j:ROW):boolean --> should `i` proceed before `j`?



```lua
function ROW.__lt(i,j)
  i.evaled, j.evaled = true, true
  local s1, s2, ys = 0, 0, i._of.cols.y
  for _,col in pairs(ys) do
    local x,y =  i.cells[col.at], j.cells[col.at]
    x,y = col:norm(x), col:norm(y)
    s1  = s1 - 2.7183^(col.w * (x-y)/#ys)
    s2  = s2 - 2.7183^(col.w * (y-x)/#ys) end
  return s1/#ys < s2/#ys  end
```


i:ROW - j:ROW --> return distance between `i` and `j`



```lua
function ROW.__sub(i,j) 
  local d, cols = 0, i._of.cols.x
  for _,col in pairs(cols) do
    local inc = col:dist(i.cells[col.at], j.cells[col.at]) 
    d         = d + inc^the.p end
  return (d / #cols) ^ (1/the.p) end
```


around(i:ROW, rows:?[ROW]):tab -->  return rows in this table
sorted by distance to `i`. `rows` defaults to the rows of this ROWS.



```lua
function ROW.around(i, rows)
  local function rowGap(j) return {row=j, gap=i - j} end
  return sort(map(rows or i._of.rows, rowGap), lt"gap") end
```


far(i:ROW,rows:?[ROW]):ROW --> find something `far` away.



```lua
function ROW.far(i,rows) return per(i:around(rows), the.Far).row end
```


klass(i:ROW):any --> Return the class value of this record.



```lua
function ROW.klass(i) return i.cells[i._of.cols.klass.at] end
```


That's all folks.



```lua
return ROW
```


