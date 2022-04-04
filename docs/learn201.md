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
return function(data)
  local tmp,xnums = {}
  local function go(c,x,    col)
    if x ~= "?" then 
      col = xnums[c]
      if col then x=(x - col.lo) // ((col.hi - col.lo+1E-32) / the.bins) end end
    return x end
```



```lua
  local function xnum(c,name) 
    if ako.xnum(name) then return {lo=1E32, hi=-1E32} end end
```



```lua
  local function train(c,x,    col) 
    col = xnums[c]
    if col and x ~= "?" then 
       col.hi = math.max(x, col.hi)
       col.lo = math.min(x, col.lo) end 
    return x end
```



```lua
  print("dat",data)
  for row in items(data) do 
    push(tmp, row) 
    if   xnums then collect(row, train) 
    else xnums = collect(row,xnum)  end end
  for j=2,#tmp do tmp[j] = collect(tmp[j], go) end
  return NB(tmp) end
