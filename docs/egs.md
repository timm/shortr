<img align=left width=175 src="/docs/head.png"> <img 
src="https://img.shields.io/badge/purpose-se,ai-informational?style=flat&logo=hyper&logoColor=white&color=red"> <img 
src="https://img.shields.io/badge/purpose-learner,optimizer-informational?style=flat&logo=hyper&logoColor=white&color=ff69b4"> <img 
src="https://img.shields.io/badge/language-lua-informational?style=flat&logo=lua&logoColor=white&color=yellow"> <img 
src="https://img.shields.io/badge/platform-osx,linux-informational?style=flat&logo=linux&logoColor=white&color=orange"> <a
href="https://github.com/timm/l5/actions/workflows/tests.yml"><img src="https://github.com/timm/l5/actions/workflows/tests.yml/badge.svg"></a> <a 
href="https://zenodo.org/badge/latestdoi/206205826"> <img src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a><br>
<small>
<b><a href="https://menzies.us/l5">Docs</a> •  <a href="https://github.com/timm/l5/blob/master/LICENSE.md">&copy;2022</a> </b>, Tim Menzies, <timm@ieee.org></small><br clear=all>


## Egs
Egs store examples (in `rows`), summarized in columns (in `cols`)

```lua
function Egs:new(names) return as({rows={}, cols=Cols(names)}, Egs) end
```



```lua
function Egs:new4file(file,  i)
  for _,row in lines(file) do if i then i:add(row) else i=Egs(row) end end
  return i end
```



```lua
function Egs.add(i,t)
  t = t.cells or t -- detail (for future extension)
  push(i.rows, map(i.cols.all, function(col) return col:add(t[col.at]) end)) end
```



```lua
function Egs.mid(i,cols) 
  return map(cols or i.cols.all, function(col) return col:mid() end) end
```


## Col
Convert  names into various Column types.

```lua
local ako={}
ako.ratio  = function(x) return x:find"^[A-Z]" end
ako.goal   = function(x) return x:find"[-+!]"  end
ako.klass  = function(x) return x:find"!$"     end
ako.ignore = function(x) return x:find":$"     end
ako.less   = function(x) return x:find"-$"     end
```


Every new column goes into `all`.  Also, for any column that we we
are not ignoring, then that also gets added to (a) either the list
of `x` independent columns or `y` dependent columns; and (b) maybe,
the `klass` slot.

```lua
function Cols:new(names)
  local i = as({names=names, klass=nil,all={}, x={}, y={}}, Cols)
  for at,name in pairs(names) do
    local col = (ako.ratio(name) and Ratio or Nominal)(at,name) 
    col.is_goal = ako.goal(name)
    push(i.all, col)
    if not ako.ignore(name) then
      if ako.klass(name) then i.klass = col end
      push(ako.goal(name) and i.y or i.x, col) end end
  return i end
```


## Nominal
Summarize symbols in `Nominal`s

```lua
function Nominal:new(at,name)
  at,name = at or 0, name or ""
  return as({at=at,name=name,n=0,has={},mode=nil,most=0},Nominal) end
```



```lua
function Nominal.add(i,x)
  if x ~= "?" then 
    i.n =i.n+1
    i.has[x] = 1 + (i.has[x] or 0) 
    if i.has[x] > i.most then i.most, i.mode = i.has[x], x end end
  return x end
```



```lua
function Nominal.mid(i) return i.mode end
```


## Ratio
Summarize numbers in `Ratio`s

```lua
function Ratio:new(at,name)
  at,name = at or 0, name or ""
  return as({at=at,name=name,n=0,mu=0,m2=0,sd=0,
             w=ako.less(name) and -1 or 1},Ratio) end
```



```lua
function Ratio.add(i,x)
  if x ~= "?" then 
    i.n =i.n+1
    local d= x - i.mu
    i.mu = i.mu + d/i.n
    i.m2 = i.m2 + d*(x - i.mu)
    i.sd = ((i.m2<0 or i.n<2) and 0) or ((i.m2/(i.n - 1))^0.5)
    i.lo = i.lo and math.min(x, i.lo) or x
    i.hi = i.hi and math.max(x, i.hi) or x end 
  return x end
```



```lua
function Ratio.mid(i) return i.mu end
```


## Return

```lua
return {Egs=Egs, Ratio=Ratio, Nominal=Nominal}
