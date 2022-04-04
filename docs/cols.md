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
local COLS = class("COLS",OBJ)
function COLS:new(names)
  self.names, self.klass   = names, nil
  self.all, self.x, self.y = {}, {}, {}
  for at,name in pairs(names) do
    local now = push(self.all, (ako.num(name) and NUM or SYM)(at,name))
    if not ako.ignore(name)  then
      if ako.klass(name) then self.klass=now end 
      push(now.indep and self.x or self.y, now) end end end
```



```lua
function COLS:add(row)
  for _,col in pairs(self.all) do col:add(row[col.at]) end
  return row end
```



```lua
return COLS
