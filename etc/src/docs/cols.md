# [:high_brightness: SHORTr : less (but better) XAI](all.md)

<a href="all.md"><img align=right width=550 src="xai4.png"></a>

AI and XAI (explainable artificial intelligence) need not be
complicated.  For example, here we need just a few 100 lines of LUA
to search N items to  find and explain the best ones, using just
log(N) evals.  

Along the way, we built an object model that could
also be applied to  many other AI tasks (nearest neighbor, decision
trees, bayes classifiers, etc).


|       what | where                                                                                                                                                                                     |
|-----------:|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| start here | [help](all.md) &hellip;  [install](/INSTALL.md) &hellip; [design notes](design.md)                                                                                                        |
|      build | [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile) (just for doco)                                                                                                   |
|      demos | [go](go.md)                                                                                                                                                                               |
|       apps | [nb](nb.md) &hellip; [tree](tree.md)                                                                                                                                                      |
|  functions | [lib](lib.md)                                                                                                                                                                             |
|    methods | [bin](bin.md) &hellip; [cols](cols.md) &hellip; [num](num.md) &hellip; [row](row.md)<br> [rows](rows.md) &hellip; [some](some.md) &hellip; [sym](sym.md) &hellip; [tree](tree.md) |

Also, this code shows just how cool is  LUA.
With very little coding, 
the code supports  TDD,  OO, several tiny DSLs,
literate programming, 
command-line
arguments,  and numerous  higher-order tricks. LUA is LISP without brackets!!

<p align=center>
<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
</p>

##  Class Col
Factor for making NUMs or SYMs

**RESPONSIBILITIES** : 
- [Create](#create) columns, keeping pointers to the dependent and independent  columns in the `y` and `x` variables.
- [Update](#update) cukumn summaries
- Knows if we want to minimize or maximize these values (see `w`).

**COLLABORATORS** :
- [NUM](num.md) , [SYM](sym.md)
------------------------------------------------------------



```lua
local all=require"all"
local obj, push = all.obj, all.push
local NUM, SYM = require"NUM", require"SYM"
```


### Create
> COLS(names:[str]) :COLS > Factory. Turns a list of names into NUMs or SYMs.<
Goal columns get added to `i.y` and others to `i.x` (unless denoted `ignored`). 
A klass column goes to `i.klass`.



```lua
local COLS = obj("COLS", function(i,names) 
  i.names = names   -- :[str]       list of known columns names
  i.all   = {}      -- :[NUM|SYM]   all the columns
  i.x     = {}      -- :[NUM|SYM]   list of pointers to just the independent columns
  i.y     = {}      -- :[NUM|SYM]   list of pointers to just the dependent columns
  i.klass = nil     -- :?(NUM|SYM)  pointer to the klass column, may be nil.
  for at,txt in pairs(names) do i:make1Column(at,txt) end end)

function COLS.make1Column(i,at,txt)
  local skipp=  function(x) return (x or ""):find":$"     end -- what to ignore
  local klassp= function(x) return (x or ""):find"!$"     end -- single goal
  local goalp=  function(x) return (x or ""):find"[!+-]$" end -- dependent column
  local nump=   function(x) return (x or ""):find"^[A-Z]" end -- NUM or SYM?
  local col =   (nump(txt) and NUM or SYM)(at,txt) 
  push(i.all, col)
  if not skipp(txt) then
    push(goalp(txt) and i.y or i.x, col)
    if klassp(txt) then i.klass = col end end end 
```


### Update
> add(i:COLS: row:ROW) > Update columns using data from `row`.<



```lua
function COLS.add(i,row)
  for _,cols in pairs{i.x,i.y} do
    for _,col in pairs(cols) do col:add(row.cells[col.at]) end end end
```


That's all folks



```lua
return COLS
```


