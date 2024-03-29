## Library Functions
**Responsbilities**
- [Linting](#linting) code;
- [List](#list) processing;
- [Mapping](#mapping) functions over lists;
- Misc [Math](#math) functions.
- Misc [Meta](#meta) tricks
- Support for [Objects](#objects)
- Parsing [Settings] from help strings, updating [Settings] from command-line.
- Coercion of [String to thing](#string-to-thing)s; e.g. reading in CSV files
- Coercion of [Thing to string](#thing-to-string)s; e.g. printing tables
- All our start-up actions are defined in a  [Tests](#tests) suite 
  (the `go` variable defined in [go](go.md)).


```lua
local m={}
```

### Linting
> rogues() > Find rogue locals. Run `rogues()` _last_ after everything else. <

```lua
local b4={}; for k,v in pairs(_ENV) do b4[k]=k end
function m.rogues()
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end end

```

###  List
> any(a:tab):any > Return any item, picked at random. <

```lua
function m.any(a, i)    i=m.R()*#a//1; i=math.max(1,math.min(i,#a)); return a[i] end

```

> many(a:tab,n:number):any > Return any `n`' items, picked at random. <

```lua
function m.many(a,n, u) u={}; for j=1,n do u[1+#u]= m.any(a) end;return u end

```

> per(t:tab, p:?float=.5) :x > Return `p`-th ranked item from `t`. <

```lua
function m.per(t,p) p=p*#t//1; return t[math.max(1,math.min(#t,p))] end

```

> push(t:tab, x:any) :x > Add `x` to end of `t`; return `x`. <

```lua
function m.push(t,x) t[1+#t] = x; return x end

```

> sort(t:tab, f:fun) :tab  > Return `t`, sorted of function `f` (default "&lt;"). <

```lua
function m.sort(t,f) table.sort(t,f); return t end

```

> splice(t:tab,start=?int=1,stop:?num=#t,step:?num=1):t > Return  items `start` to `stop`, stepping by `step`. <

```lua
function m.splice(t, start, stop, step)
  local u={}
  start = (start or 1)//1
  stop  = (stop or #t)//1
  step  = (step or  1)//1 
  for j=start,stop,step do u[1+#u]=t[j] end
  return u end

```

## Mapping
> map(t:tab, f:fun): tab  
 kap(t:tab, f:fun): tab  
 maps(list1:tab, list2:tab, f:fun): tab   
 kaps(list1:tab, list2:tab, f:fun): tab > 
Return items in `t`, filtered thru `f`.
If `f` returns nil, then the output table shrinks. `kap` and `kaps` pass the
key and value to `f`. `maps` and `kaps` pass items from two lists. <

```lua
function m.map(t,f,     u) u={};for _,x in pairs(t) do u[1+#u]=f(x) end;return u end
function m.kap(t,f,     u) u={};for k,x in pairs(t) do u[1+#u]=f(k,x) end;return u end
function m.maps(t,u,f,  v) v={};for k,x in pairs(t) do v[1+#v]=f(x,u[k]) end;return v end
function m.kaps(t,u,f,  v) v={};for k,x in pairs(t) do v[1+#v]=f(k,x,u[k]) end;return v end

```

> sum(t:tab, f:?fun=same): num > sum items in `t`, filtered through `fun` <

```lua
function m.sum(t,f,   u) 
   u=0;for _,x in pairs(t) do u=u+(f or m.same)(x) end; return u end

```

### Maths
> big:num > Return a big number. <

```lua
m.big = math.huge

```

> R(max:?num=1):num > Return a random number `0..max`. <

```lua
m.R = math.random

```

> rnd(x:num, places:int):num  > Return `x` rounded to some number of `place`  &#9312; . <

```lua
function m.rnd(x, places)  --   &#9312;
  local mult = 10^(places or 2)
  return math.floor(x * mult + 0.5) / mult end


```

> rnds(t:num, places:?int=2):num > Return items in `t` rounds to `places`. <

```lua
function m.rnds(t, places)
  local u={};for k,x in pairs(t) do u[k]=m.rnd(x,places or 2)end;return u end

```

> small(min,x) :num > Defined what `small` means. <

```lua
function m.small(min,x) return math.max(4,min<1 and x^min or x) end

```

### Meta
> gt(x:str):function   
  lt(x:str):function > Returns functions that sorts on `x`.i < 

```lua
function m.lt(x) return function(a,b) return a[x] < b[x] end end
function m.gt(x) return function(a,b) return a[x] > b[x] end end

```

> same(x:any):any > Return x, unchanged. <

```lua
m.same=function(x) return x end

```

### Objects
> obj(name:str, fun:fun):object > Return a klass `name` with constructor `fun`. <
Add a unique `id` and a `tosting` method (that uses `cat` (above).

```lua
local _id = 0
function m.obj(name,fun,    t,new,x)
  function new(kl,...) _id=_id+1; x=setmetatable({_id=_id},kl);fun(x,...); return x end 
  t = {__tostring=m.cat,_is=name}; t.__index=t
  return setmetatable(t, {__call=new}) end

```

### Settings
> cli(t:tab) :tab > For keys in `t`, look for updates on command-line.  <
Things with boolean defaults are flipped via `--flag`. 
Other keys need `--flag value`.  Print the help
(if `-h` appears on command line). Return a table with setting `key`s and
`value`s. IMPORTANT NOTE: this function alters-in-place the table `t`
that is passed in-- which means that it alters settings for anything pointing
to `t`.

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

> opts(x:str) :tab > Parse `str` for lines with `--`; then pull keys+defaults. <

```lua
function m.opts(x)
  local t = {}
  x:gsub("\n  ([-][^%s]+)[%s]+([-][-]([^%s]+))[^\n]*%s([^%s]+)",
           function(f1,f2,k,x) t[k] = m.thing(x) end)
  t._HELP = x
  return t end

```

### String to thing
> csv(file:str,  fun:fun):tab > Call `fun` with lines, split on ",". <
coercing strings to nums, bools, etc (where appropriate).

```lua
function m.csv(file,fun)
  m.lines(file, function(line) fun(m.words(line, ",", m.thing)) end) end 

```

> lines(file:str,  fun:fun):tab > Call `fun` with lines. <

```lua
function m.lines(file, fun)
  local file = io.input(file)
  while true do
    local line = io.read()
    if not line then return io.close(file) else fun(line) end end end

```

> thing(s:str):any > Coerce string to whatever is simplest. <

```lua
function m.thing(x)
  x = m.trim(x)
  if x=="true" then return true elseif x=="false" then return false else
    return math.tointeger(x) or tonumber(x) or x end  end

```

> trim(s:str) : str > Trim leading and trailing white space. <

```lua
function m.trim(x) return  x:match"^%s*(.-)%s*$" end

```

> words(s:str, sep:str, fun:fun):tab > Return `t` filled with `s`, split  on `sep`. <

```lua
function m.words(s,sep,fun,      t)
   fun = fun or m.same
   t={};for x in s:gmatch(m.fmt("([^%s]+)",sep)) do t[1+#t]=fun(x) end; return t end

```

### Thing to string
> cat(t:tab):str  > Return table as string. For key-indexed lists, show keys (sorted). <

```lua
function m.cat(t,    key,u)
  function key(k,v) if (tostring(k)):sub(1,1)~="_" then return m.fmt(":%s %s",k,v) end end
  u=  #t>1 and m.map(t,f or tostring) or m.sort(m.kap(t,key))
  return (t._is or "").."{"..table.concat(u," ").."}" end

```

> chat(t:tab) > Print table (as string). Return `t`. <

```lua
function m.chat(t) print(m.cat(t)); return t end

```

> chunks(file:str) > divide source code into comments and code. <

```lua
function m.chunks(file)
  local b4,now,t = 0,0,{}
  local hints=function(s)  -- Emphasize type hints comments (those with "> .* > .* <")
          return s:gsub("> ([^>]*) > ([^<]*) <",function(hint,txt)
            hint = m.trim(hint):gsub("([%w]+):","`%1`:")
                               :gsub("([A-Z][A-Z]+)",function(word)
                                  local down=word:lower()
                                  return "["..word.."]("..down..".md#create)" end)
            return '> ***'..hint .. "***<br>\n"..m.trim(txt).."\n" end ) 
  end ------------------------
  local dump = function(what,t) 
    if t[#t]:find"^[%s]*$" then t[#t]=nil end -- zap trailing blank line
    local s= table.concat(t,"\n")             -- build text dump
    print(what==0 and (hints(s).."\n") or ("\n\n```lua\n"..s.."\n```\n\n")) 
  end --------------------
  m.lines(file, function(s)
    now = b4
    if s:sub(1,3)=="-- " then now=0; s=s:sub(4) elseif s:find"^%S" then now=1 end
    if now==b4 then t[1+#t]=s else dump(b4,t); t={s} end
    b4 = now end)
  dump(now,t) end 

```

> fmt(s:str,...) :str > Emulate printf.  <

```lua
m.fmt=string.format

```

### Tests
> on(opts:tab, tests:[fun]) > Run some tests. <
If  `opt.go=="all"`, then run all tests, sorted on their name.
Before each test, reset random seed and the options in `opts`.

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

That's all folks.

```lua
return m
```

