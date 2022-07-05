# [:high_brightness: B(ai)T = a tiny XAI library, in LUA](all.md)

<a href="all.md"><img align=right width=350 src="bat2.png"></a>

LUA is a "batteries-not-included" language.   But LUA makes it easy to add in the missing bits.
E.g. here are my "B(ai)tteries" for XAI (explainable artificial intelligence).   



|       what | where                                                                                                         |
|-----------:|---------------------------------------------------------------------------------------------------------------|
| start here | [help](all.md) &vert;  [install](/INSTALL.md) &vert; [design notes](design.md)     |                                                                 |
|      build | [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile) (just for doco)                       |
|      demos | [go](go.md)                                                                                                   |
|       apps | [nb](nb.md) &vert; [tree](tree.md)                                                                                |
|  functions | [lib](lib.md)                                                                                                 |
|    methods | [bin](bin.md) &vert; [cols](cols.md) &vert; [num](num.md) &vert; [row](row.md) &vert; [rows](rows.md) &vert; [some](some.md) &vert; [sym](sym.md) &vert; [tree](tree.md) |

<br clear=all>
<p align=center>
<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
</p>

## ROWS to tree



```lua
local all = require"all"
local chat,lt,map    = all.chat,all.lt, all.map
local small,sort,the = all.small, all.sort,all.the
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


