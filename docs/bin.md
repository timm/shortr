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
local BIN=class("BIN",OBJ)
function BIN:new(at,name, lo,hi,ys) 
  self.at, self.name        = at or 0, name or ""
  self.lo, self.hi, self.ys = lo, hi or lo, ys or SYM() end
```



```lua
function BIN:_tostring()
  local x,lo,hi,big = self.name, self.lo, self.hi. math.huge
  if     lo ==  hi  then return fmt("%s == %s",x, lo)  
  elseif hi ==  big then return fmt("%s >= %s",x, lo)  
  elseif lo == -big then return fmt("%s <  %s",x, hi)  
  else                   return fmt("%s <= %s < %s",lo,x,hi) end end
```



```lua
function BIN:select(row)
  local x, lo, hi = row[self.at], self.lo, self.hi
  return x=="?" or lo == hi and lo == x or lo <= x and x < hi end
```



```lua
function BIN:add(x,y)
  if x<self.lo then self.lo = x end 
  if x>self.hi then self.hi = x end 
  self.ys:add(y) end
```



```lua
function BIN.mergeSameDivs(b4,after)
  local merged = b4.ys:merged(after.ys)
  if merged then
   return BIN(b4.at, b4.name, b4.lo, after.hi, merged) end end
```



```lua
function BIN.mergeNext(b4,after)
  if b4.hi == after.lo then
   return BIN(b4.at, b4.name, b4.lo, after.hi, b4.ys:merge(after.ys)) end end
```



```lua
return BIN
```


23 45  {:left 100 :right 45} n=145
