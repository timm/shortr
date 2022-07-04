<img align=left width=250   src="bat2.png">

# B(Ai)ttery
LUA is a "batteries-not-included" language.
But LUA makes it easy to add in the  missing bits.
E.g. here are some "b(Ai)tteries" for XAI.

(c) 2022, Tim Menzies<br><timm@ieee.org>
<br clear=all>

|config | build | demos | apps | functions | classes |
|-------|-------|-------|------|-----------|---------|
|[all](all.html)|[Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile)<br>(just for doco)|[go](go.html)|[nb](nb.html)|[lib](lib.html)|[bin](bin.html) :: [cols](cols.html) :: [num](num.html) :: [row](row.html) :: [rows](rows.html) :: [some](some.html) :: [sym](sym.html)|

<a href=".."><img src="https://img.shields.io/badge/Language--lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 


nb.lua
## Bayes classifier


<details><summary></summary>

```lua
local all=require"all"
local big,cat,chat,csv,gt = all.big,all.cat,all.chat,all.csv,all.gt
local map,obj,push,sort,the = all.map, all.obj, all.push, all.sort, all.the
local ROWS = require"ROWS"

--> NB(src=(str|tab), report=?function=print) -> classify examples
```

</details>


(but hold off till we've seen at least `the.wait` examples).


<details><summary></summary>

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

</details>


