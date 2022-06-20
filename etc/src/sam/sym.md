<img align=left width=150 src="https://raw.githubusercontent.com/timm/shortr/master/docs/img/cup.png">
   
shortr :: [home](home) :: [asd](as) :: [sdsa](asd) :: [sad](asd)  
    
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a><br clear=all>

 
# Sym


```lua
local Sym={}
function Sym.NEW(at,txt) --> (at :num, txt:str) :Sym
  return {_is=Sym,at=at or 0,txt=txt or {},n=0,kept={},
         w=(txt or ""):find"-$" and -1 or 1} end
```
## Creation
```lua
function Sym.add(i,x,inc) --> (i :Sym, x :any, inc :?num) :Sym
  if x=="?" then return else
    i.n = i.n + 1
    i.kept[x] = inc + (i.kept[x] or 0) 
    if i.kept[x] > i.most then i.most,i.mode = i.kept[x],x end end end
```
## Queries
```lua
function [Sym mid](Sym.mid)(i) --> (i :Sym) :num
  return i.mode end

function Sym.div(i,  e) --> (i :Sym) :num
  local log=function(x) return math.log(x,2) end
  e=0; for _,v in pairs(i.kept) do if v>0 then e=e-v/i.n*log(v/i.n) end end
  return e end

return Sym
```
