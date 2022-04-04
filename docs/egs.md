<a name=top>&nbsp;<br>
<img align=left width=175 src="/docs/head.png"> <img 
src="https://img.shields.io/badge/purpose-se,ai-informational?style=flat&logo=hyper&logoColor=white&color=red"> <img 
src="https://img.shields.io/badge/language-lua-informational?style=flat&logo=lua&logoColor=white&color=yellow"> <img 
src="https://img.shields.io/badge/platform-osx,linux-informational?style=flat&logo=linux&logoColor=white&color=orange"> <a
href="https://github.com/timm/l5/actions/workflows/tests.yml"><img src="https://github.com/timm/l5/actions/workflows/tests.yml/badge.svg"></a> <a 
href="https://zenodo.org/badge/latestdoi/206205826"> <img src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a><br>
<b> <a href="https://github.com/timm/l5/blob/master/LICENSE.md">&copy;2022</a> </b>, Tim Menzies, <timm@ieee.org>
<h1>The Little LUA Learning Library</h1><br clear=all>



```lua
local EGS = class("EGS",OBJ)
function EGS:new() 
  self.rows, self.cols = {}, nil end
```



```lua
function EGS:adds(data)
  for row in items(data) do self:add(row) end
  return self end
```



```lua
function EGS:add(row)
  if not self.cols then self.cols = COLS(row)
                   else push(self.rows, self.cols:add(row)) end end
```



```lua
function EGS.mid(i,cols)
   return map(cols or i.cols.y, function(col) return col:mid() end) end
```



```lua
function EGS:div(cols)
   return map(cols or i.cols.y, function(col) return col:div() end) end
```



```lua
function EGS:clone(rows)
  local out = EGS(self.cols.name)
  for _,row in pairs(rows or {}) do out:add(row) end
  return out end
```



```lua
function EGS:dist(row1,row2)
  local d, n = 0, 0
  for _,col in pairs(self.cols.x) do 
    n = n + 1
    d = d + col:dist(row1[col.at], row2[col.at])^the.p end 
  return (d/n) ^ (1/the.p) end
```



```lua
function EGS:better(row1,row2)
  local s1, s2, n, e = 0, 0, #self.cols.y, math.exp(1)
  for _,col in pairs(self.cols.y) do
    local a = norm(col.lo, col.hi, row1[col.at] )
    local b = norm(col.lo, col.hi, row2[col.at] )
    s1      = s1 - e^(col.w * (a - b) / n)
    s2      = s2 - e^(col.w * (b - a) / n) end
  return s1 / n < s2 / n  end
```



```lua
function EGS:bins(other)
  local out = {}
  for n,col1 in pairs(self.cols.x) do
    tmp=col1:bins(other.cols.x[n],BIN)
    if #tmp > 1 then for _,bin in pairs(tmp) do push(out,bin) end end end end
```



```lua
function EGS:bestRest()
  self.rows = sort(self.rows, function(a,b) return self:better(a,b) end) 
  local n = (#self.rows)^the.best
  return slice(self.rows, 1,          n),      -- top n things
               many( self.rows, n*the.rest, n+1) end -- some sample of the rest
```


function egs.xplain(i)
 best, rest = egs.bestRest(i)
 return egs.contrasts(i, best,rest) end

```lua

return EGS 
