<a name=top></a>

# :high_brightness: B(Ai)ttery

<img align=left width=400 src="bat2.png">

LUA is a "batteries-not-included" language.   
But LUA makes it easy to add in the  missing bits.   
E.g. here are some "b(Ai)tteries" for XAI (explainable artifical intelligence).   

(c) 2022, Tim Menzies <timm@ieee.org>

|what          | where |
|-------------:|:------|
|**config**    | [all](all.md#top)   |
|**build**     | [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile) (just for doco)  | 
|**demos**     | [go](go.md#top)  |
|**apps**      | [nb](nb.md#top), [tree](tree.md#top)  |
|**functions** | [lib](lib.md#top) |  
|**methods**   | [bin](bin.md#top), [cols](cols.md#top), [num](num.md#top), [row](row.md#top), [rows](rows.md#top), [some](some.md#top), [sym](sym.md#top) |

In this code,  **ROW** holds one record while **ROWS** holds lots of **ROWs**. Each **ROWS** summarizes numeric
or symbolic  columns in **NUM**s or **SYM**s, respectively. These summaries are held in **COLS** which divide the columns into (x,y) sets (for
independent and dependent columns, respectively). Pairs of (x,y) columns are summarized in **BIN**s (and adjacent **BIN**s that have similar y distributions
are merged). 
**SOME** is a helper
class for **NUM**s that retains some sample of all the numerics in that column. i

Everything else is just tiny extensions to the above object model. e.g. 
decision **TREE**s are built by recursively finding the **BIN**s that best distinguish different **ROW**s. 

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


 ##  info on 2 cols



```lua
local all=require"all"
local big,fmt,lt,obj = all.big,all.fmt,all.lt,all.obj,all
local small,sort = all.small,all.sort

--> BIN(xlo:num,xhi:num,ys:(NUM|SYM)):BIN ->
```


`ys` stores values seen from `xlo to `xhi`.



```lua
local BIN = obj("BIN", function(xlo, xhi, ys)
  i.lo, i.hi, i.ys = xlo, xhi, ys end)
```


add(`i` :Bin, `x` :num, `y` :(num|str) -> Ensure `lo`,`hi` covers `x`. Add `y` to `ys`.



```lua
function BIN.add(i,x,y)
  i.lo = math.min(i.lo, x)
  i.hi = math.max(i.hi, x)
  ys:add(y) end

function BIN.hold(i, row)
  local x = row.cells[i.ys.at]
  if x=="?" or i.lo==i.hi or i.lo<x and x<=i.hi then return row end end

function BIN.holds(i, rows)
  return map(rows, function(row) return i:hold(row) end) end

function BIN.merged(i,j, min)
  local a, b, c = i.ys, j.ys, i.ys:merge(j.ys)
  if a.n < min or b.n < min or c:div() <= (a.n*a:div() + b.n*b:div())/c.n then
    return BIN(i.lo, j.hi, c) end end

function BIN.show(i)
  local x,lo,hi = i.ys.txt, i.lo, i.hi
  if     lo ==  hi  then return fmt("%s == %s", x, lo)
  elseif hi ==  big then return fmt("%s >  %s", x, lo)
  elseif lo == -big then return fmt("%s <= %s", x, hi)
  else                   return fmt("%s <  %s <= %s", lo,x,hi) end end
```


----



```lua
function BIN.BINS(rows,col,yKlass,y)
  y      = y or function(row) return row:klass() end
  yKlass = yKlass or SYM
  local n,list, dict = 0,{}, {}
  for _,row in pairs(rows) do
    local v = row.cells[col.at]
    if v ~= "?" then
      n = n + 1
      local pos = col:bin(v)
      dict[pos] = dict[pos] or push(list, BIN(v,v,yKlass(col.at, col.txt)))
      dict[pos]:add(v, y(row)) end end
  list = col:merges(sort(list, lt"lo"), small(the.Min, n))
  return {bins= list,
          div = sum(list,function(z) return z.ys:div()*z.ys.n/n end)} end
```


