# [:high_brightness: B(Ai)ttery](all.md)

<a href="all.md"><img align=left width=300 src="bat2.png"></a>

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


## Bayes classifier



```lua
local all=require"all"
local big,cat,chat,csv,gt = all.big,all.cat,all.chat,all.csv,all.gt
local map,obj,push,sort,the = all.map, all.obj, all.push, all.sort, all.the
local ROWS = require"ROWS"

--> NB(src=(str|tab), report=?function=print) -> classify examples
```


(but hold off till we've seen at least `the.wait` examples).



```lua
local NB = obj("NB", function (i,src,report)
  i.overall, i.dict, i.list = nil, {},{}
  chat(i)
  report = report or print
  local iterate = type(src)=="table" and map or csv
  iterate(src, function(row)
    if not i.overall then i.overall = ROWS(row)  else -- (0) eat row1
      row = i.overall:add(row)  -- XX add to overall
      if #i.overall.rows > the.wait then report(row:klass(), i:guess(row)) end
      i:train(row) end end)              -- add tp rows's klass
  end)

--> train(i:NB,row:ROW):atom -> ensure klass exists, add `row` to that klass
function NB.train(i,row)
  local kl = row:klass()
  i.dict[kl]     = i.dict[kl] or push(i.list,i.overall:clone()) --klass is known
  i.dict[kl].txt = kl                     -- each klass knows its name
  i.dict[kl]:add(row) end                  -- update klass with row

--> guess(i:NB,row:ROW):atom -> return symbol of klass with max likelihood
function NB.guess(i,row)
  local most,out = -big, nil
  for key,rows in pairs(i.dict) do
    local tmp = rows:like(row, #i.list,#i.overall.rows)
    if tmp > most then most,out = tmp,key end end
  if not out then out = sort(i.list,
                             function(a,b) return #a.rows>#b.rows end)[1].txt end
  return out end

return NB
```


