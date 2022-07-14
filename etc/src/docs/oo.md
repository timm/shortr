<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a> 
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 


# [:high_brightness: SHORTr : less (but better) XAI](oo.md)
<img align=right width=600 src="xai4.jpeg"> 
AI and XAI (explainable artificial intelligence) need not be hard. 
E.g. here's a few hundred lines of LUA to search N items to find and 
explain the best ones, using just log(N) evals.

This code starts with a help string (from which we extract our global settings)
and ends with a library of demos (see the `go` functions at end of file).  
- Each setting can be (optionally) updated by a command-line flag.
- Demos can be run separately or  all at once (using `-g all`).
  To handle regression tests, we report back to the operating system 
  the failures seen when the demos run.

This code uses the following classes.
- ROWS hold many ROWs which are summarized in COLs.
- COLs can be either SYMbolic or NUMeric). 
- Two helper classes are:
  - SOME: keeps a sample of data from a NUMeric column.
  - BIN:  tracks what goal variables are seen within some range.

Data from disk is read into a ROWS, from which we 
do some clustering (and each cluster is new ROWS object, containing a subset
of the data). A decision tree is built that reports the difference between the 
better and worst classes (and that tree is nothing but tree of ROWS with a `kids` pointer
to sub-ROWS).

More technically, using the independent variables, we do recursive random projections using 
[FASTMAP](https://www.ijcai.org/Proceedings/2018/0198.pdf#page=2) and [Aha's heterogeneous 
distance measures](https://link.springer.com/content/pdf/10.1007/BF00153759.pdf#page=6). 
At each level of the recursion, pairs of
remote points are ranked (using the dependent variables) and all
the data associated with the best/worst points are labeled `bests`
or `rests`.  Supervised discretization and an entropy-based
decision tree is then used to distinguish the best `bests` from
the worst `rests`. Note that all this access the dependent variables just _log2(N)_ times.


|Category|Class|Protocol|What|Notes|
|:---------|:----|:--------|:---|:----|
|Config |  |  | [***help  :str***](#)|Help text for this code.|
| |  |  | [***the  :table***](#1)|Config settings. Extracted from `help`. e.g. `the.cohen=.35`.|
| |  |  | [***cli(the :tab) :tab***](#2)|Updates settings from the command line.|
|Names |  |  | [***obj(txt :str,base :?class)  :class***](#3)|Make a class, perhaps as a kid of `base`.|
|Columns | COL | Create | [***COL(at :?int=0, txt :?str="") : COL***](#4)|Superclass constructor for columns.|
| |  | Update | [***add(x :any, inc :?int=1)***](#5)|`inc` times repeat: add `x`|




## Config
> ***help  :str***<a id=1></a><br>Help text for this code. 


```lua
local help = [[

SHORTr: semi-supervised multi-objective optimization XAI
(c) 2022 Tim Menzies <timm@ieee.org> BSD2 license
     
From N items, find and explain the best ones, using just log(N) evals.
PASS1 (guess): eval two distant items on multi-objective criteria.
      Prune everything nearest the worst one. Recurse on rest.  
PASS2 (guess again): do it again, using better items from first pass.  
PASS3 (explain): recursively discretize attributes on how well they
      distinguish the best and worst items (seen in second pass).
   
USAGE:
  lua shortr.lua [OPTIONS]
   
OPTIONS:
  -M  Min    min size of space                    =  .5
  -b  bins   max number of bins                   =  16
  -F  Far    how far to look for remove points    =  .95
  -k  k      Bayes hack: low attribute frequency  =  2
  -m  m      Bayes hack: low class frequency      =  1
  -p  p      distance coefficient (2=Euclidean)   =  2
  -s  seed   random number seed                   =  10019
  -S  Some   max number of nums to keep           =  256
  -w  wait   wait this number before testing      =  10
   
OPTIONS (other):
  -f  file   file           = ../../data/auto93.csv
  -g  go     start-up goal  = nothing
  -h  help   show help      = false ]]

```

> ***the  :table***<a id=2></a><br>Config settings. Extracted from `help`. e.g. `the.cohen=.35`. 


```lua
local the={}
local function thing(x) 
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x end 

help:gsub("\n[%s]+[-]%S[%s]+([%S]+)[^\n]+=[%s]*([%S]+)",function(k,x) the[k]=thing(x) end)

```

> ***cli(the :tab) :tab***<a id=3></a><br>Updates settings from the command line. 

e.g. `-c .2` -- updates `the.cohen`. To flip booleans, just mention them 
on the command line; e.g. `-h` will flip `the.help=false` to `the.help=true`.

```lua
local function cli(t)
  for key,x in pairs(t) do 
    x = tostring(x)
    for n,flag in ipairs(arg) do 
      if   flag=="-"..key:sub(1,1) 
      then x = x=="false" and "true" or x=="true" and "false" or arg[n+1] end end
    t[key] = thing(x) end 
  return t end

```

## Names
`b4` is a list of names known before this code. Used by `rogue()` (see below)

```lua
local b4={}; for k,v in pairs(_ENV) do b4[k]=k end
```

By defining names before the code, the code can be written in any order.

```lua
local cat,chat,csv,fmt,kap,lines,map
local new,obj,per,push,R,rogues,same,sort,trim,words

```

> ***obj(txt :str,base :?class)  :class***<a id=4></a><br>Make a class, perhaps as a kid of `base`. 

Instances have a unique `id` and use the `cat` function for pretty printing.
Every class must have a `CLASS:new()` function.

```lua
local _id=0
function obj(txt,base,  t,new,i)
  function new(k,...) 
    _id=_id+1; i=setmetatable({_id=id},k); k.new(i,...); return i end
  t={__tostring=cat}
  for k,v in pairs(base or {}) do t[k] = v end
  t.is, t.__index =  txt, t
	return setmetatable(t,{__call=new}) end

local COL,ROW,ROWS   = obj"COL", obj"ROW", obj"ROWS"
local NUM, SOME, SYM = obj("NUM",COL), obj("SOME",COL), obj("SYM",COL) 

```

## Columns
### COL
#### Create
> ***COL(at :?int=0, txt :?str="") : COL***<a id=5></a><br>Superclass constructor for columns. 


```lua
function COL:new(at,txt)
  self.at  = at or 0     
  self.txt = txt or ""  
  self.n   = 0 end     

```

#### Reports
> dist(x:any, y:any) :num > Return distance. For missing values, assume max distance. <

```lua
function COL:dist(x,y)
  return x=="?" and y=="?" and 1 or self:dist1(x,y) end

```

#### Update
> ***add(x :any, inc :?int=1)***<a id=6></a><br>`inc` times repeat: add `x` 


```lua
function COL:add(x,inc)
  if x ~= "?" then 
    inc = inc or 1
    self.n = self.n + inc
    self:add1(x,inc) end end

```

### SOME
#### Create

```lua
function SOME:new(...)
  COL.new(self, ...)
  self.kept,self.ok,self.max,self.n = {},true,the.Some,0  end

```

#### Update

```lua
function SOME:add1(x,inc)
  for j=1,inc do
    local a= self.kept
    if     #a  < self.max        then self.ok=false; push(a,x) 
    elseif R() < self.max/self.n then self.ok=false; a[R(#a)]=x end end end 

```

#### Reports

```lua
function SOME:has()
  self.kept = self.ok and self.kept or sort(self.kept)
  self.ok=true
  return self.kept  end

```

### NUM
#### Create

```lua
local NUM=obj("NUM",COL)
function NUM:new(...)
  COL.new(self, ...)
  self.kept = SOME()          
  self.w = self.txt:find"-$" and -1 or 1 end

```

#### Report

```lua
function NUM.div(i) 
  local a=i.kept:has(); return (per(a,.9) - per(a,.1))/2.56 end

function NUM.mid(i) 
  local a=i.kept:has(); return per(a,.5) end

function NUM:norm(x)
  local a = self.kept:has()
  return (a[#a]-a[1])<1E-9 or (x-a[1])/(a[#a]-a[1]) end

```

#### Update

```lua
function NUM:add1(x,inc)
  for j=1,inc do self.kept:add(x) end end 

```

## Lib
### Lint
> rogues() > Warn if our code introduced a rogue global. <

```lua
local function rogues()
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end end
```

### Maths

```lua
R=math.random
```

### Lists

```lua
function same(x)      return x end
function map(t,f,  u) u={};for _,x in pairs(t)do u[1+#u]=f(x) end;return u end
function kap(t,f,  u) u={};for k,x in pairs(t)do u[1+#u]=f(k,x)end;return u end
function sort(t,f)    table.sort(t,f); return t end
function push(t,x)    t[1+#t]=x; return x end
function per(t,p)     p=p*#t//1; return t[math.max(1,math.min(#t,p))] end


```

### Misc

```lua
function same(x) return x end
```

### String2things


```lua
function csv(file,fun)
  lines(file, function(line) fun(words(line, ",", thing)) end) end 

function lines(file, fun)
  local file = io.input(file)
  while true do
    local line = io.read()
    if not line then return io.close(file) else fun(line) end end end

```

> the :table > Config settings. Extracted from `help`. <

```lua
function thing(x) 
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x end 


function words(s,sep,fun,      t)
  fun = fun or same
  t={};for x in s:gmatch(fmt("([^%s]+)",sep)) do t[1+#t]=fun(x) end; return t end

```

### Thing2string

```lua
function chat(t) print(cat(t)); return t end
function cat(t,   u,pub) 
  pub=function(k,v) return tostring(k):sub(1,1)~="_" end
  if type(t)~="table" then return tostring(t) end
  if #t>0             then return "{"..table.concat(map(t,tostring)," ").."}" end
  u={};for k,v in pairs(t) do if pub(k) then u[1+#u]=(":%s %s"):format(k,v) end end
  table.sort(u)
  return (t.is or "").."{"..table.concat(u," ").."}"  end

fmt=string.format

```

### Testing

```lua
local go,fails={},0

function go.all() 
  local defaults={}
  for k,v in pairs(the) do defaults[k]=v end 
  local want = function(k,_)if k~="all" then return k end end
  for k,x in pairs(sort(kap(go,want))) do 
    for k,v in pairs(defaults) do the[k]=v end 
    math.randomseed(the.seed)
    if true ~= go[x]() then 
      print("FAIL:",k)
      fails=fails+1 end end end

function go.the()  chat(the);    return true end
function go.some( n) 
  n = NUM()
  chat(n)
  for j=1,10^3 do n:add(j) end
  chat(n.kept:has()) return true end

```

## Start
This code can get used in two ways.   
- If used in `lua shortr.lua` then it is _top-level_ code.   
  In this case, this code is in control and it will call
  one or more of the `go` demos.
- If used in `require "shortr"` then it a _included_ code. 
  In this case, something else will control how this is used.

```lua
if    pcall(debug.getlocal, 4, 1) -- true if this is an included call.
then  return {ROWS=ROWS, the=the}  
else  the=cli(the)
      if the.help then print(help) elseif go[the.go] then go[the.go]() end
      rogues()
      os.exit(fails)
end
```

