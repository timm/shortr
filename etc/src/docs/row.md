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


## hold 1 record
See also [ROWS](rows.html) that holds multiple records.  
And [NUM](num.html) and [SYM](sym.html) that summarize the 
columns of the records.


<details><summary></summary>

```lua
local all = require"all"
local big,chat,lt,map  = all.big, all.chat, all.lt, all.map
local obj,rnds,sort    = all.obj, all.rnds, all.sort

--> ROW(of:ROWS, cells:tab) :ROW -> Place to store one record
```

</details>


(and stats on how it is used; e.g. `i.evaled=true` if we touch the y values.


<details><summary></summary>

```lua
local ROW = obj("ROW", function(i,eden,cells) 
  i._eden,i.cells,i.evaled = eden,cells,false end)

--> i:ROW - j:ROW -> return distance between `i` and `j`
function ROW.__sub(i,j) 
  local d, cols = 0, i._eden.cols.x
  for _,col in pairs(cols) do
    local inc = col:dist(i.cells[col.at], j.cells[col.at]) 
    d         = d + inc^the.p end
  return (d / #cols) ^ (1/the.p) end

--> around(i:ROW, rows:?[ROW]):tab ->  return rows in this table
```

</details>


sorted by distance to `i`. `rows` defaults to the rows of this ROWS.


<details><summary></summary>

```lua
function ROW.around(i, rows)
  local function rowGap(j) return {row=j, gap=i - j} end
  return sort(map(rows or i._eden.rows, rowGap), lt"gap") end

--> better(i:ROW, j:ROW):boolean -> should `i` proceed before `j`?
function ROW.__lt(i,j)
  i.evaled, j.evaled = true, true
  local s1, s2, ys = 0, 0, i._eden.cols.y
  for _,col in pairs(ys) do
    local x,y =  i.cells[col.at], j.cells[col.at]
    x,y = col:norm(x), col:norm(y)
    s1  = s1 - 2.7183^(col.w * (x-y)/#ys)
    s2  = s2 - 2.7183^(col.w * (y-x)/#ys) end
  return s1/#ys < s2/#ys  end

--> far(i:ROW,rows:?[ROW]):ROW -> find something `far` away.
function ROW.far(i,rows) return per(Row.around(i,rows), the.Far).row end

--> klass(i:ROW):any -> Return the class value of this record.
function ROW.klass(i) return i.cells[i._eden.cols.klass.at] end

return ROW
```

</details>


