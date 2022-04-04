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
local COL = class("COL",OBJ)
function COL:new(at,name)
   self.at, self.name = at or 0, name or ""
   self.n       = 0
   self.ignorep = ako.ignore(self.name)
   self.indep   = not ako.goal(self.name)
   self.w       = self.name:find"-$" and -1 or 1 end
```



```lua
function COL:adds(t) 
  for _,x in pairs(t) do self:add(x) end; return self end
```



```lua
function COL:add(x,inc)
  if x ~= "?" then
    inc = inc or 1
    self.n = self.n + inc
    self:add1(x,inc) end
  return x end
```



```lua
function COL:dist(x,y)
  return x=="?" and y=="?" and 1 or self:dist1(x,y) end 
```



```lua
function COL:merged(other,    out)
  out = self:merge(other)
  if out:div()*.95 <= (self.n*self:div() + other.n*other:div())/out.n then
    return out end end
```



```lua
return COL
