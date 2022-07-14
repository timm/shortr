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
|Config |  |  | [***help  :str***](#1)|Help text for this code.|
| |  |  | [***the  :table***](#2)|Config settings. Extracted from `help`. e.g. `the.cohen=.35`.|
| |  |  | [***cli(the :tab) :tab***](#3)|Updates settings from the command line.|
|Names |  |  | [***obj(txt :str,base :?class)  :class***](#4)|Make a class, perhaps as a kid of `base`.|
|Columns | COL | Create | [***COL(at?int=0, txt :?str="") : COL***](#5)|Superclass constructor for columns.|
| |  |  | [***clone()  :COL --> Return some of the same structure.***](#6)||
| |  | Query | [***dist(x :any, y :any)  :num***](#7)|Return distance. For missing values, assume max distance.<|
| |  | Update | [***add(x :any, inc :?int=1)***](#8)|`inc` times repeat: add `x`|
| | SOME | Create | [***SOME(at?int=0, txt :?str="") : SOME***](#9)|Constructor.|
| |  | Update | [***add(i :SOME : x :num)***](#10)|If full then at odds `i.some/i.n`, keep `x`(replacing some older item, at random). Otherwise, just add.|
| |  | Query | [***has(i :SOME) :tab***](#11)|Ensure contents are sorted. Return those contents.|
| | NUM | Create | [***NUM(at :?num=0, txt :?str="")  :NUM***](#12)|Constructor.|
| |  | Discretize | [***bin(x :any)***](#13)|Return `x` mapped to a finite number of bins|
| |  |  | [***merge(j :NUM)  :NUM***](#14)|merge two NUMs|
| |  |  | [***merges(t :[BIN])  :[BIN]***](#15)|merge a list of BINs (for numeric y-values)|
| |  | Distance | [***dist(x :num,y :num) : num***](#16)|Return distance 0..1 between `x,y`.|
| |  | Likelihood | [***like(x :any)***](#17)|Return the likelihood that `x` belongs to `i`. <|
| |  | Query | [***div(i :NUM)  :tab***](#18)|Return `div`ersity of a column (tendency to depart central tendency).|
|Lib | Maths | Update | [***big :num***](#19)|Return `math.huge`|
| |  |  | [***R(n :?num=1)***](#20)|If `n` missing return a random number 0..1. Else return 1..`n`.|
| | Lists |  | [***kap(t :tab,f :fun) :tab***](#21)|Filter key,values through `fun`. Remove slots where `fun` returns nil|
| |  |  | [***map(t :tab,f :fun) :tab***](#22)|Filter through `fun`. Remove slots where `fun` returns nil|
| |  |  | [***per(t :tab,p :float) :any***](#23)|Returns the items `p`-th way through `t`.|
| |  |  | [***sort(t :tab,f :fun) :tab***](#24)|Sort list in place. Return list. `fun` defaults to `<`.|
| |  |  | [***sort(t :tab,f :fun) :tab***](#25)|Sort list in place. Return list. `fun` defaults to `<`.|
| | Misc |  | [***ako(x) :tab***](#26)|Return arg's metatable.|
| |  |  | [***same(x) :x***](#27)|Return arg, un changed.|
|Testing | Thing2string |  | [***go.all()***](#28)|Runs all the tests (called from command-line by `-g all`).|




## Config
<img align=right width=150 src="cdown.png">

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
<img align=right width=150 src="ndown.png">
`b4` is a list of names known before this code. Used by `rogue()` (see below)

```lua
local b4={}; for k,v in pairs(_ENV) do b4[k]=k end
```

By defining names before the code, the code can be written in any order.

```lua
local ako,big,cat,chat,csv,fmt,isa,kap,lines,map
local new,obj,per,push,R,rogues,same,sort,trim,words

```

> ***obj(txt :str,base :?class)  :class***<a id=4></a><br>Make a class, perhaps as a kid of `base`. 

Identity, methods, inheritance, polymorphism, encapsulation, all in 8 lines :-).

Instances have a unique `id` and use the `cat` function for pretty printing.
Every class must have a `CLASS:new()` function. 
Also, inheritance is implemented by copying over the parent methods
(so the parent has to be fully implemented before calling `obj`).

```lua
local _id=0
function obj(txt,base,  t,new,i)
  function new(k,...) 
    _id=_id+1; i=setmetatable({_id=_id},k); k.new(i,...); return i end
  t={__tostring=cat,super=base}
  for k,v in pairs(base or {}) do t[k] = v end
  t.is, t.__index =  txt, t
	return setmetatable(t,{__call=new}) end

```

## Columns
### COL
Superclass of NUM and SYM. <img align=right width=150 src="c.png">

**RESPONSIBILITIES** : 
- Create or clone a duplicate structure 
- Discretize values into a few bins (for building trees)
- Distance calculations (for clustering)
- Likelihood calculations (for Bayes)
- Query  central tendency and diversity and other things
- Update summarization
#### Create
> ***COL(at?int=0, txt :?str="") : COL***<a id=5></a><br>Superclass constructor for columns. 


```lua
local COL=obj"COL"
function COL:new(at,txt)
  self.at  = at or 0     
  self.txt = txt or ""  
  self.n   = 0 end     

```

> ***clone()  :COL --> Return some of the same structure.***<a id=6></a><br> 


```lua
function COL:clone()
  return ako(self)(self.at, self.txt) end

```

#### Query
> ***dist(x :any, y :any)  :num***<a id=7></a><br>Return distance. For missing values, assume max distance.< 


```lua
function COL:dist(x,y)
  return x=="?" and y=="?" and 1 or self:dist1(x,y) end

```

#### Update
> ***add(x :any, inc :?int=1)***<a id=8></a><br>`inc` times repeat: add `x` 


```lua
function COL:add(x,inc)
  if x ~= "?" then 
    inc = inc or 1
    self.n = self.n + inc
    self:add1(x,inc) end end

```

### SOME
<img align=right width=150 src="s.png">
Given a finite buffer of some small size `max`, then after reading 
a very large set of `n` numbers, we should only be keeping `max/n` of those nums.
#### Create
> ***SOME(at?int=0, txt :?str="") : SOME***<a id=9></a><br>Constructor. 


```lua
local SOME=obj("SOME",COL)
function SOME:new(...)
  self.super.new(self, ...)
  self.kept, self.ok, self.max = {}, true, the.Some end

```

#### Update
> ***add(i :SOME : x :num)***<a id=10></a><br>If full then at odds `i.some/i.n`, keep `x`(replacing some older item, at random). Otherwise, just add. 


```lua
function SOME:add1(x,inc)
  for j=1,inc do
    local a= self.kept
    if     #a  < self.max        then self.ok=false; push(a,x) 
    elseif R() < self.max/self.n then self.ok=false; a[R(#a)]=x end end end 

```

#### Query
> ***has(i :SOME) :tab***<a id=11></a><br>Ensure contents are sorted. Return those contents. 


```lua
function SOME:has()
  self.kept = self.ok and self.kept or sort(self.kept)
  self.ok=true
  return self.kept  end

```

### NUM
Summarize a sequence of numbers.
<img align=right width=150 src="n.png">

**RESPONSIBILITIES** : 
- Same as COL and knows if we want to minimize or maximize these values (see `w`).

**COLLOBERATIONS** : 
- Uses SOME to keep a sample of the data seen.

#### Create
> ***NUM(at :?num=0, txt :?str="")  :NUM***<a id=12></a><br>Constructor. 

Q: Where we use the `w` weight?  
A: See the `better` method inside [ROW](row.md) where `w` is used to 
   weight the dependent variables. In that code, one ROW is better than another
   when that weight rewards changing to that value.

```lua
local NUM=obj("NUM",COL)
function NUM:new(...)
  self.super.new(self, ...)
  self.kept = SOME()          
  self.w = self.txt:find"-$" and -1 or 1 end

```

#### Discretize
To discretize a numeric column, first map all the numbers into a finite number
of bins (say, divided on "(hi-lo)/16"). Then look at the class distrubutions
in each bin. While two adjacent bins have similar distributions, then merge them
and go look for anything else that might be merged. 

Q: For that to work, don't you need to to collect information on _two_ columns.
   (one you are trying to discretize and another holding the class distribution)?   
A:  Yes indeed. The class [BIN](bin.md) does that. Here, we define some services to help
[BIN](bin.md) do its work.

> ***bin(x :any)***<a id=13></a><br>Return `x` mapped to a finite number of bins 


```lua
function NUM:bin(x)
  local a = self.kept:has()
  local b = (a[#a] - a[1])/the.bins
  return a[#a]==a[1] and 1 or math.floor(x/b+.5)*b end

```

> ***merge(j :NUM)  :NUM***<a id=14></a><br>merge two NUMs 


```lua
function NUM:merge(j,     k)
  k = self:clone()
  for _,kept in pairs{self.kept, j.kept} do
    for _,x in pairs(kept) do k:add(x) end end
  return k end

```

> ***merges(t :[BIN])  :[BIN]***<a id=15></a><br>merge a list of BINs (for numeric y-values) 

Note the last line of `merges`: if anything merged, then loop again looking for other merges.
Else, time to finish up (expand the bins to cover all gaps across the number line).
FYI, to see what happens when this code calls `merged`, goto [BIN](bin.md).

Q: why is this defined here (and not in the BIN class)?  
A: The `merges` of several
BINs is different for NUMs and SYMs (in SYMs, we can't merge anything so `merges` just 
returns the original list, unchanged).

```lua
function NUM:merges(b4, min) 
  local function fillInTheGaps(bins)
    bins[1].lo, bins[#bins].hi = -big, big
    if #bins>1 then
      for n=2,#bins do bins[n].lo = bins[n-1].hi end end
    return bins 
  end ------------- 
  local n,now = 1,{}
  while n <= #b4 do
    local merged= n<#b4 and b4[n]:merged(b4[n+1],min) --"merged" defined in bin.md
    now[#now+1] = merged or b4[n]
    n           = n + (merged and 2 or 1)  -- if merged, skip passed the merged bin
  end
  return #now < #b4 and self:merges(now,min) or fillInTheGaps(now) end

```

#### Distance
> ***dist(x :num,y :num) : num***<a id=16></a><br>Return distance 0..1 between `x,y`. 

This code assume max distance for missing values.

```lua
function NUM:dist(x,y)
  if x=="?" and y=="?" then return 1 end
  if     x=="?" then y = self:norm(y); x = y<.5 and 1 or 0 
  elseif y=="?" then x = self:norm(x); y = x<.5 and 1 or 0
  else   x,y = self:norm(x), self:norm(y) end
  return math.abs(x - y) end 

```

#### Likelihood
> ***like(x :any)***<a id=17></a><br>Return the likelihood that `x` belongs to `i`. < 


```lua
function NUM:like(x,...)
  local sd,mu=self:div(), self:mid()
  if sd==0 then return x==mu and 1 or 1/big end
  return math.exp(-.5*((x - mu)/sd)^2) / (sd*((2*math.pi)^0.5)) end  

```

#### Query
> ***div(i :NUM)  :tab***<a id=18></a><br>Return `div`ersity of a column (tendency to depart central tendency). 


<img align=right src="normal.png"> 

Q: In the code for `div`, where does the magic number 2.56 come from?   
A: Intuitively, the diversity can be measured by (a) ignoring outliers from, say, the top
and bottom 10% then (b) reporting the high-low values of the rest, perhaps divided by two
(since we reporting divergence from some middle point). 
But with a little bit of mathemagic, we can turn that (90th-10th)/2 report into some more
standard.
Recall that &pm;1 to &pm;2 sds covers 68 to 95% of the Gaussian prob.
In between, at &pm;1.28, we cover 90%. So (p90-p10)/(2*1.28) returns one sd. 

TL;DR, to make statisticians happy, do not
divide by 2, but 2*1.28 = 2.56.

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
<img align=right width=150 src="l.png">
Musc utils.
### Lint
> rogues() > Warn if our code introduced a rogue global. <

```lua
local function rogues()
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end end
```

### Maths
> ***big :num***<a id=19></a><br>Return `math.huge` 


```lua
big = math.huge
```

> ***R(n :?num=1)***<a id=20></a><br>If `n` missing return a random number 0..1. Else return 1..`n`. 


```lua
R = math.random
```

### Lists
> ***kap(t :tab,f :fun) :tab***<a id=21></a><br>Filter key,values through `fun`. Remove slots where `fun` returns nil 


```lua
function kap(t,f,  u) u={};for k,x in pairs(t)do u[1+#u]=f(k,x)end;return u end

```

> ***map(t :tab,f :fun) :tab***<a id=22></a><br>Filter through `fun`. Remove slots where `fun` returns nil 


```lua
function map(t,f,  u) u={};for _,x in pairs(t)do u[1+#u]=f(x) end;return u end

```

> ***per(t :tab,p :float) :any***<a id=23></a><br>Returns the items `p`-th way through `t`. 


```lua
function per(t,p)  p=p*#t//1; return t[math.max(1,math.min(#t,p))] end

```

> ***sort(t :tab,f :fun) :tab***<a id=24></a><br>Sort list in place. Return list. `fun` defaults to `<`. 


```lua
function sort(t,f) table.sort(t,f); return t end

```

> ***sort(t :tab,f :fun) :tab***<a id=25></a><br>Sort list in place. Return list. `fun` defaults to `<`. 


```lua
function push(t,x) t[1+#t]=x; return x end

```

### Misc
> ***ako(x) :tab***<a id=26></a><br>Return arg's metatable. 


```lua
function ako(x) return getmetatable(x) end

```

> ***same(x) :x***<a id=27></a><br>Return arg, un changed. 


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

## Testing
Code for demos, tests.   <img align=right width=150  src="tpurple.png"> 
To disable a test, move it from `go` to `no`.

```lua
local go,no,fails={},{},0

```

> ***go.all()***<a id=28></a><br>Runs all the tests (called from command-line by `-g all`). 

Resets `the` and the random number seed before each call. 

```lua
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

```

Show the settings

```lua
function go.the()  chat(the);    return true end

```

SOME

```lua
function go.some( s) 
  the.Some = 16
  s = SOME()
  for j=1,10^3 do s:add(j) end
  local m=0
  for _,n in pairs(s:has()) do if n> 200 and n<300 then m =m+1 end end
  return m>1 and m<12 end

```

NUM

```lua
function go.num( n,n1) 
  n  = NUM(6,"tim")
  n1 = n:clone()
  for j=1,10^3 do n:add(j) end
  for j=1,10^3 do n1:add(j) end
  chat(n.kept:has())
  chat(n1.kept:has())
  chat(n1)
  return true end

```

## Start
This code can get used in two ways.   <img align=right width=150  src="sgreen.png"> 
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

