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


## ROWS to tree



```lua
local all = require"all"
local map,small,the = all.map,all.small, all.the
local ROWS = require"rows"
local BIN = require"bin"
local SYM = require"sym"

function ROWS.tree(i, listOfRows)
  local labels, root = {}, i:clone()
  for label,rows1 in ipairs(listOfRows) do
    for _,row in pairs(rows1) do
      root:add(row)
      labels[row._id]=label end end                 -- set label
  local function y(row) return labels[row._id] end -- get label
  return root:kids(2 * small(the.Min, #root.rows), y) end

function ROWS.kids(i, stop, y)
  if #i.rows >= stop then
    local all  = map(i.cols.x, function(xcol) 
                                 return BIN.BINS(i.rows,xcol,SYM,y) end) 
    local best = sort(all, lt"div")[1]
    i.kids     = map(best.bins, function (bin)
                                  local new = i:clone(bin:holds(i.rows))
                                  if #new.rows < #i.rows then
                                    new.gaurd = bin
                                    return new:kids(stop, y) end end) end
  return i end


function ROWS.branches(i,lvl)
  lvl = lvl or 0
  local gaurd = i.gaurd and i.gaurd:show()
  print(fmt("%-40s", cat(i:mids(i))), ("| "):rep(lvl) .. (gaurd or ""))
  for _,kid in pairs(i.kids or {}) do 
    kid:branches(1+lvl) end end
  

return ROWS
```

