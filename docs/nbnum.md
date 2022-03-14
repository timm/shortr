<a name=top>&nbsp;<br>
<img align=left width=175 src="/docs/head.png"> <img 
src="https://img.shields.io/badge/purpose-se,ai-informational?style=flat&logo=hyper&logoColor=white&color=red"> <img 
src="https://img.shields.io/badge/language-lua-informational?style=flat&logo=lua&logoColor=white&color=yellow"> <img 
src="https://img.shields.io/badge/platform-osx,linux-informational?style=flat&logo=linux&logoColor=white&color=orange"> <a
href="https://github.com/timm/l5/actions/workflows/tests.yml"><img src="https://github.com/timm/l5/actions/workflows/tests.yml/badge.svg"></a> <a 
href="https://zenodo.org/badge/latestdoi/206205826"> <img src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a><br>
<b> <a href="https://github.com/timm/l5/blob/master/LICENSE.md">&copy;2022</a> </b>, Tim Menzies, <timm@ieee.org>
<h1>The Little LUA Learning Library</h1><br clear=all>


## Add likelihood calculators

```lua
function Egs.like(i,t,prior)
  local like = prior
  for at,x in pairs(t) do
    local col = i.cols.all[at]
    if not col.is_goal then
      like = like * (x=="?" and 1 or i.cols.all[at]:like(x,prior)) end end 
  return like end
```



```lua
function Ratio.like(i,x,prior)
  if x < i.mu - 3*i.sd then return 0 end
  if x > i.mu + 3*i.sd then return 0 end
  local denom = (math.pi*2*i.sd^2)^.5
  local nom   =  math.exp(1)^(-(x-mu)^2/(2*i.sd^2+1E-32))
  return nom/(denom + 1E-32) end
```



```lua
function Nominal.like(i,x,prior) 
  return ((i.has[x] or 0) + the.M*prior)/(i.n + the.M) end 
```


## Create and update

```lua
function Nb:new() 
  return {h={}, all=nil, nh=0, n=0, wait=the.wait, log={}} end
```



```lua
function Nb:new4file(file) 
  for row in lines(file) do i:add(row) end end
```



```lua
function Nb.add(i,row)
  if not i.all then i.all = Egs(row) else i:test(row); i:train(row) end end 
```


## Train, test, classify

```lua
function Nb.train(i,t)
  i.n = i.n + 1
  local h = i.all:klass(t)
  if not i.h[h] then i.nh = i.nh + 1; i.h[h] = i.all:clone() end
  i.h[h]:add(row) 
  i.all:add(row) end
```



```lua
function Nb.test(i,t)
  if i.n > i.wait then push(i.log, {want=i.all:klass(t), got=classify(i,t)}) end end
```



```lua
function Nb.classify(i,t)
  local hi,out = -1
  for klass,h in pairs(i.h) do 
    local prior = (h.n + the.K) / (i.n + the.K*i.nh)
    local like  = h:like(t,prior)
    if like > hi then hi,out=like,klass end end
  return out end
```


## Score

```lua
function Nb.score(i,    n)
  n=0; for _,x in pairs(i.log) do if x.want==x.got then n=n+1 end end
  return n/#i.log end 
```


## Return

```lua
return {Nb=Nb, Ratio=Ratio, Nominal=Nominal}
