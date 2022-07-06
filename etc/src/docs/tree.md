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

(start here  ([help](all.md) ([install](/INSTALL.md) ([design notes](design.md)))))                                                                                               
(build       ([Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile)-- just for doc))                                                                           
(demos       ([go](go.md))                                                                                                                                                      
(methods    ([bin](bin.md) ([cols](cols.md) ([num](num.md) ([row](row.md) ([rows](rows.md) ([some](some.md) ([sym](sym.md) ([tree](tree.md))))))))))


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

## TREEs = nested ROWS
THis code has no decision tree class. Instead, we say trees are just ROWS
with a list of `kids` (and each kid is another ROW).



```lua
local all = require"all"
local cat,chat,fmt,lt,map= all.cat, all.chat,all.fmt,all.lt, all.map
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
  return root:children(2 * small(the.Min, #root.rows), y) end

function ROWS.children(i, stop, y)
  if #i.rows >= stop then
    local all  = map(i.cols.x, function(xcol) 
                     return BIN.BINS(i.rows,xcol,SYM,y) end) 
    local best = sort(all, lt"div")[1]
    i.kids     = map(best.bins, function (bin)
                  local new = i:clone(bin:holds(i.rows))
                  if #new.rows < #i.rows then
                    new.gaurd = bin
                    return new:children(stop, y) end end) end
    return i end


function ROWS.branches(i,lvl)
  lvl = lvl or 0
  local gaurd = i.gaurd and i.gaurd:show()
  print(fmt("%-40s", cat(i:mids())), ("| "):rep(lvl) .. (gaurd or ""))
  for _,kid in pairs(i.kids or {}) do 
    kid:branches(1+lvl) end end
```


That's all folks.



```lua
return ROWS
```


