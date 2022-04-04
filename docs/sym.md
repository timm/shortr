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
local SYM = class("SYM",COL)
function SYM:new(at,name)   
  self:super(at,name)
  self.has, self.most, self.mode = {}, 0, nil end
```



```lua
function SYM:add1(x,inc)
  self.has[x] = inc + (self.has[x] or 0) 
  if self.has[x] > self.most then 
    self.mode, self.most = x, self.has[x] end end 
```



```lua

function SYM:mid()      return self.mode end
function SYM:div()      return ent(self.has, self.n) end
function SYM:same(x,y)  return x==y end
```



```lua
function SYM:dist1(x,y) 
  return self:same(x,y)  and 0 or 1 end
```



```lua
function SYM:like1(x,prior)
  return ((i.has[x] or 0) + the.M*prior)/(self.n + the.M) end
```



```lua
function SYM:merge(other,      out)
  out = SYM(self.at, self.name)
  for x,n in pairs(self.has)  do out:add(x,n) end
  for x,n in pairs(other.has) do out:add(x,n) end
  return out end
```



```lua
function SYM:bins(other, BIN)
  local out = {}
  local function known(x) out[x] = out[x] or BIN(self.at, self.name, x,x) end
  for x,n in pairs(self.has)  do known(x); out[x].ys:add("left", n) end
  for x,n in pairs(other.has) do known(x); out[x].ys:add("right", n) end
  return map(slots(out), function(k) return out[k] end) end
```



```lua
return SYM
