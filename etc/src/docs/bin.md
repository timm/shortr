<a href="all.md"><img align=right width=350 src="bat2.png"></a>

# [:high_brightness: B(Ai)ttery](all.md)


LUA is a "batteries-not-included" language.   But LUA makes it easy to add in the missing bits.
E.g. here are some "b(Ai)tteries" for XAI (explainable artifical intelligence).   

<small>

|what      | where |
|---------:|:------|
|config    | [all](all.md)   |
|build     | [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile) (just for doco)  | 
|demos     | [go](go.md)  |
|apps      | [nb](nb.md), [tree](tree.md)  |
|functions | [lib](lib.md) |  
|methods   | [bin](bin.md), [cols](cols.md), [num](num.md), [row](row.md), [rows](rows.md), [some](some.md), [sym](sym.md) |

<br clear=all>
<p align=center>
<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
</p>

 ##  info on 2 cols



```lua
local all=require"all"
local the=all.the
local big,fmt,lt,obj,push = all.big, all.fmt, all.lt, all.obj, all.push
local small,sort = all.small,all.sort

--> BIN(xlo:num,xhi:num,ys:(NUM|SYM)):BIN ->
```


`ys` stores values seen from `xlo to `xhi`.



```lua
local BIN = obj("BIN", function(i, xlo, xhi, ys)
  i.lo, i.hi, i.ys = xlo, xhi, ys end)
```


add(`i` :Bin, `x` :num, `y` :(num|str) -> Ensure `lo`,`hi` covers `x`. Add `y` to `ys`.



```lua
function BIN.add(i,x,y)
  i.lo = math.min(i.lo, x)
  i.hi = math.max(i.hi, x)
  i.ys:add(y) end

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


That's all folks.



```lua
return BIN
```


