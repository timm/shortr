<img align=right width=250   src="bat2.png">

# B(Ai)ttery
LUA is a "batteries-not-included" language.
But LUA makes it easy to add in the  missing bits.
E.g. here are some "b(Ai)tteries" for XAI.

(c) 2022, Tim Menzies<br><timm@ieee.org>
<br clear=all>

|what          | where |
|:-------------|:------|
|**config**    | [all](all.html)   |
|**build**     | [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile) (just for doco)  | 
|**demos**     | [go](go.html)  |
|**apps**      | [nb](nb.html)  |
|**functions** | [lib](lib.html) |  
|**methods**   | [bin](bin.html) :: [cols](cols.html) :: [num](num.html) :: [row](row.html) :: [rows](rows.html) :: [some](some.html) :: [sym](sym.html) |

<a href=".."><img src="https://img.shields.io/badge/Language--lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 


## Library Functions


<details><summary></summary>

```lua
local m={}
```

</details>


### Linting

> ***rogues()***<br>:mag:  Find rogue locals. Run `rogues()` _last_ after everything else.


<details><summary></summary>

```lua
local b4={}; for k,v in pairs(_ENV) do b4[k]=k end
function m.rogues()
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end end
```

</details>


### Meta

> ***lt(`x` :str):function***<br>:mag:  
> ***gt(`x` :str):function***<br>:mag:  Returns functions that sorts on `x`


<details><summary></summary>

```lua
function m.lt(x) return function(a,b) return a[x] < b[x] end end
function m.gt(x) return function(a,b) return a[x] > b[x] end end
```

</details>


> ***same(`x` :any):any***<br>:mag:  Return x, unchanged.


<details><summary></summary>

```lua
m.same=function(x) return x end
```

</details>


### Maths

> ***`big` :num***<br>:mag:  return a big number


<details><summary></summary>

```lua
m.big = math.huge
```

</details>


> ***R(`max` :?num=1):num***<br>:mag:  return a random number `0..max`.


<details><summary></summary>

```lua
m.R = math.random
```

</details>


> ***rnd(`x` :num, `places` :int):num***<br>:mag:  return `x` rounded to some number of `places`.


<details><summary></summary>

```lua
function m.rnd(x, places)
  local mult = 10^(places or 2)
  return math.floor(x * mult + 0.5) / mult end

function m.small(min,x) return min<1 and n^min or x end
```

</details>


> ***rnds(`t` :num, `places` :?int=2):t***<br>:mag:  return items in `t` rounds to `places`.


<details><summary></summary>

```lua
function m.rnds(t, places)
  local u={};for k,x in pairs(t) do u[k]=m.rnd(x,places or 2)end;return u end
```

</details>


###  Lists

> ***splice(`t` :tab,start=?int=1,`stop` :?num=#t,`step` :?num=1):t***<br>:mag:  pull items
`start` to `stop`, stepping by `step`. 


<details><summary></summary>

```lua
function m.splice(t, start, stop, step)
  local u={}
  for n=(start or 1)//1,(stop or #t)//1,(step or 1)//1 do u[1+#u]=t[n] end
  return u end
```

</details>


> ***sort(`t` :tab, `f` :fun) :tab***<br>:mag:  Return `t`, sorted of function `f` (default "<").


<details><summary></summary>

```lua
function m.sort(t,f) table.sort(t,f); return t end
```

</details>


> ***push(`t` :tab, `x` :any) :x***<br>:mag:  Add `x` to end of `t`; return `t`.


<details><summary></summary>

```lua
function m.push(t,x) t[1+#t] = x; return x end
```

</details>


> ***per(`t` :tab, `p` :?float=.5) :x***<br>:mag:  Return `p`-th ranked item from `t`.


<details><summary></summary>

```lua
function m.per(t,p) p=p*#t//1; return t[math.max(1,math.min(#t,p))] end
```

</details>


> ***map(`t` :tab, `f` :fun): tab***<br>:mag:  
> ***kap(`t` :tab, `f` :fun): tab***<br>:mag:  
> ***maps(`list1` :tab, `list2` :tab, `f` :fun): tab***<br>:mag:  
> ***kaps(`list1` :tab, `list2` :tab, `f` :fun): tab***<br>:mag:  Return items in `t`, filtered thru `f`.
If `f` returns nil, then the output table shrinks. `kap` and `kaps` pass the
key and value to `f`. `maps` and `kaps` pass items from two lists.


<details><summary></summary>

```lua
function m.map(t,f,     u) u={};for _,x in pairs(t) do u[1+#u]=f(x) end;return u end
function m.kap(t,f,     u) u={};for k,x in pairs(t) do u[1+#u]=f(k,x) end;return u end
function m.maps(t,u,f,  v) v={};for k,x in pairs(t) do v[1+#v]=f(x,u[k]) end;return v end
function m.kaps(t,u,f,  v) v={};for k,x in pairs(t) do v[1+#v]=f(k,x,u[k]) end;return v end
```

</details>


### String to thing

> ***thing(`s` :str):any***<br>:mag:  Coerce string to whatever
is simplest (boolean or integer or float or, if all else fails, a string).


<details><summary></summary>

```lua
function m.thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false else
    return math.tointeger(x) or tonumber(x) or x end  end
```

</details>


> ***words(`s` :str, `sep` :str, `fun` :fun):tab***<br>:mag:  Return `t` filled with `s`, split  on `sep`.


<details><summary></summary>

```lua
function m.words(s,sep,fun,      t)
   fun = fun or m.same
   t={};for x in s:gmatch(m.fmt("([^%s]+)",sep)) do t[1+#t]=fun(x) end; return t end
```

</details>


lines(`file` :str,  `fun` :fun):tab ---> Call `fun` with lines


<details><summary></summary>

```lua
function m.lines(file, fun)
  local file = io.input(file)
  while true do
    local line = io.read()
    if not line then return io.close(file) else fun(line) end end end
```

</details>


csv(`file` :str,  `fun` :fun):tab ---> Call `fun` with lines, split on ",", 
coercing strings to nums, bools, etc (where appropriate).


<details><summary></summary>

```lua
function m.csv(file,fun)
  m.lines(file, function(line) fun(m.words(line, ",", m.thing)) end) end 
```

</details>


### Thing to string

fmt(`s` :str,...) :str ---> emulate prinft


<details><summary></summary>

```lua
m.fmt=string.format
```

</details>


cat(`t` :tab):str ---> Return table as string. For key-indexed lists, show keys (sorted).


<details><summary></summary>

```lua
function m.cat(t,    key,u)
  function key(k,v) if (tostring(k)):sub(1,1)~="_" then return m.fmt(":%s %s",k,v) end end
  u=  #t>1 and m.map(t,f or tostring) or m.sort(m.kap(t,key))
  return (t._is or "").."{"..table.concat(u," ").."}" end
```

</details>


chat(`t` :tab):t ---> Print table (as string). Return `t`.


<details><summary></summary>

```lua
function m.chat(t) print(m.cat(t)); return t end
```

</details>


> ***chunks(`file` :str)***<br>:mag:  divide source code into comments and code.


<details><summary></summary>

```lua
function m.chunks(file)
  local b4,now,t = 0,0,{}
  local hints=function(s) 
     return s:gsub("([%w]+):","`%1` :") 
             :gsub("([^\n]+) [-][-]>([^\n]+)","> ***%1***<br>:mag: %2") 
  end ------------------------
  local show = function(what,t) 
    if t[#t]:find"^[%s]*$" then t[#t]=nil end
    local s= table.concat(t,"\n") 
    print(what==0 and (hints(s).."\n") or (
          "\n<details><summary></summary>\n\n```lua\n" 
          ..s.."\n```\n\n</details>\n\n")) 
  end --------------------
  m.lines(file, function(s)
    now = b4
    if s:sub(1,3)=="-- " then now=0; s=s:sub(4) elseif s:find"^%S" then now=1 end
    if now==b4 then t[1+#t]=s else show(b4,t); t={s} end
    b4 = now end)
  show(now,t) end 
```

</details>


### Settings

> ***opts(`x` :str) :tab***<br>:mag:  Parse `str` for lines with `--`; then pull keys+defaults. 


<details><summary></summary>

```lua
function m.opts(x)
  local t = {}
  x:gsub("\n  ([-][^%s]+)[%s]+([-][-]([^%s]+))[^\n]*%s([^%s]+)",
           function(f1,f2,k,x) t[k] = m.thing(x) end)
  t._HELP = x
  return t end
```

</details>


> ***cli(`t` :tab) :tab***<br>:mag:  For keys in `t`, look for updates on command-line. 
Things with boolean defaults are flipped via `--flag`. 
Other keys need `--flag value`.  Print the help
(if `-h` appears on command line). Return a table with setting `key`s and
`value`s. IMPORTANT `NOTE` : this function alters-in-place the table `t`
that is passed in-- which means that it alters settings for anything pointing
to `t`.


<details><summary></summary>

```lua
function m.cli(t)
  for key,x in pairs(t) do 
    x = tostring(x)
    local long, short = "--"..key, "-"..key:sub(1,1)
    for n,flag in ipairs(arg) do 
      if flag==short or flag==long then
        x = x=="false" and "true" or x=="true" and "false" or arg[n+1] 
        t[key] = m.thing(x) end end end
  if t.help then os.exit(print(t._HELP:gsub("[%u][%u%d]+","\27[1;32m%1\27[0m"),"")) end 
  return t end
```

</details>


### Tests

> ***on(`opts` :tab, `tests` :[fun])***<br>:mag:  Run some tests.
If  `opt.go=="all"`, then run all tests, sorted on their name.
Before each test, reset random seed and the options `opts.


<details><summary></summary>

```lua
function m.on(opts,tests)
  local fails, old = 0, {}
  for k,v in pairs(opts) do old[k]=v end
  local t=opts.go=="all" and m.kap(tests,function(k,_) return k end) or {opts.go}
  for _,txt in pairs(m.sort(t)) do
    local fun = tests[txt]
    if type(fun)=="function" then
      for k,v in pairs(old) do opts[k]=v end -- reset opts to default
      math.randomseed(opts.seed or 10019)    -- reset seed to default
      local out = fun()
      if out ~= true then fails=fails+1
                          print(m.fmt("FAIL: [%s] %s",txt,out or "")) end end end
  m.rogues()
  os.exit(fails) end -- if fails==0 then our return code to the OS will be zero.
```

</details>


### Objects

> ***obj(`name` :str, `fun` :fun):object***<br>:mag:  Return a klass `name` with constructor `fun`.
Add a unique `id` and a `tosting` method (that uses `cat` (above).


<details><summary></summary>

```lua
local _id = 0
function m.obj(name,fun,    t,new,x)
  function new(kl,...) _id=_id+1; x=setmetatable({_id=_id},kl);fun(x,...); return x end 
  t = {__tostring=m.cat,_is=name}; t.__index=t
  return setmetatable(t, {__call=new}) end
-----
```

</details>


### Return


<details><summary></summary>

```lua
return m
```

</details>


