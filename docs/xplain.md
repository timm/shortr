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
local XPLAIN=class("XPLAIN",OBJ)
function XPLAIN:new(left,right)
  local out = {}
  for n,col1 in pairs(left.cols.x) do
    tmp=col1:bins(right.cols.x[n],BIN)
    if #tmp > 1 then for _,bin in pairs(tmp) do push(out,bin) end end end end
```



```lua
local RULE=class("RULE",OBJ) 
function XPLAIN:asRule(bins)
  local out={}
  for _,b in pairs(bins) do out[b.at]=rule[b.at] or {}; push(out[b.at],bin) end
  return out end
   
function XPLAIN:like(rule, klass, h, n) -- h={"true"=100, "false"=40} n=100+40
  fs = {}
  for at,bins in pairs(rule) do
    fs[at] = 0
    for _,bin in paris(bins) do fs[at] = fs[at] + (bin.ys[klass] or 0) end end
  local prior = ((h[klass] or 0) + the.K) / (n + the.K * 2)
  local out   = math.log(prior)
  for _,v in pairs(fs) do out=out+math.log((v+the.M*prior)/(h[klass]+the.M)) end
  return out end
```



```lua
local z=1E-32
XPLAIN.rules = {}
local goal   = XPLAIN.rules
function goal.optimize(b,r) return (b<r or (b+r)>.01) and 0 or b^2/(b+r+z) end
function goal.monitor( b,r) return (r<b or (b+r)>.01) and 0 or r^2/(b+r+z) end
function goal.tabu(    b,r) return 1/(b+r+z) end
