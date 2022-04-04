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
local rule={}
function rule.new(bins,   t)
  t = {}
  for key,one in pairs(bins) do 
    t[one.at]=t[one.at] or{}; push(t[one.at],one) end 
  return {bins=t} end
```



```lua
function rule.selects(i,row)
  local function ors(bins)
    for key,x in pairs(bins) do if bin.select(x,row) then return true end end
    return false end
  for at,bins in pairs(i.bins) do if not ors(bins) then return false end end
  return true end 
```



```lua
function rule.show(i,bins)
  local cat, order, ors
  cat =  function(t,sep) return table.concat(t,sep) end
  order= function(a,b)  return a.lo < b.lo end
  ors=   function(bins) 
          return cat(map(bin.Merges(sort(bins,order)),bin.show)," or ") end
  return cat(map(i.bins, ors)," and ") end
```



```lua
return rule
