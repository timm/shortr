<img align=left width=250   src="bat2.png">

# B(Ai)ttery
LUA is a "batteries-not-included" language.
But LUA makes it easy to add in the  missing bits.
E.g. here are some "b(Ai)tteries" for XAI.

(c) 2022, Tim Menzies<br><timm@ieee.org>
<br clear=all>

**config:** [all](all.html);
**build:** [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile)
(just for doco);
**demos:** [go](go.html);
**apps:** [nb](nb.html);
**functions:** [lib](lib.html)    
**klasses:** [bin](bin.html)
:: [cols](cols.html)
:: [num](num.html)
:: [row](row.html)
:: [rows](rows.html)
:: [some](some.html)
:: [sym](sym.html)

<img src="https://img.shields.io/badge/Language--lua-%232C2D72.svg?logo=lua&logoColor=white"> <a
href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 


rows.lua
## store many ROWs


<details><summary></summary>

```lua
local all = require"all"
local chat,csv,map,obj  = all.chat, all.csv, all.map,  all.obj
local push,rnd,rnds,the = all.push, all.rnd, all.rnds, all.the
local COLS,ROW          = require"COLS",require"ROW"

--> ROWS(names:?[str], rows:?[ROW}) :ROWS -> Place to store many ROWS
```

</details>


 and summarize them (in `i.cols`).


<details><summary></summary>

```lua
local ROWS = obj("ROWS", function(i,names,rows) 
  i.rows, i.cols = {}, (names and COLS(names) or nil)
  i.eden = i
  for _,row in pairs(rows or {}) do i:add(row) end end)

--> add(i:ROWS: row:ROW) -> add ROW to ROWS, update the summaries in `i.cols`.
function ROWS.add(i,t) 
  t = t.cells and t or ROW(i.eden,t)
  if i.cols then i.cols:add(push(i.rows, t)) else i.cols=COLS(t.cells) end 
  return t end

--> ROWS.clone(init:?[ROW]) :ROWS -> Return a ROWS with same structure as `i`. 
```

</details>


Optionally, `init`ialize it with some rows. Add a pointer back to the 
original table that spawned `eve`rything else (useful for some distance calcs).


<details><summary></summary>

```lua
function ROWS.clone(i,init)
  local j=ROWS(i.cols.names,init)
  j.eden = i.eden 
  return j end

--> fill(i:ROWS: src:(str|tab)):ROWS -> copy the data from `src` into `i`.
function ROWS.fill(i,src)
  local iterate = type(src)=="table" and map or csv
  iterate(src, function(t) i:add(t) end) 
  return i end

--> like(i:ROWS,row;ROW,nklasses:num,nrows:num):num -> Return 
```

</details>


P(H)*&prod;<sub>i</sub> (P(E<sub>i</sub>|H)). Do it with logs
to handle very small numbers.


<details><summary></summary>

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

</details>


rounded to `p` places.


<details><summary></summary>

```lua
function ROWS.mids(i,p,cols) 
  local t={}
  for _,col in pairs(cols or i.cols.y) do t[col.txt]=col:mid(p) end
  return rnds(t,p or 2) end

return ROWS
```

</details>

