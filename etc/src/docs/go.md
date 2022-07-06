<a href="all.md"><img align=right width=400 src="xai4.png"></a>

# [:high_brightness: SHORTr : less (but better) XAI](all.md)


AI and XAI (explainable artificial intelligence) need not be
hard.  E.g. here's a few 100 lines of LUA
to search N items to  find and explain the best ones, using just
log(N) evals.  

**start here:**  ([help](all.md) ([install](/INSTALL.md) ([design notes](design.md))))                                                                                               
**build:**       ([Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile)-- just for doc)                                                                           
**demos:**       ([go](go.md))                                                                                                                                                      
**apps:**         ([nb](nb.md) ([tree](tree.md)))   
**functions:**   ([lib](lib.md))     
**methods:**    ([bin](bin.md) ([cols](cols.md) ([num](num.md) ([row](row.md) ([rows](rows.md) ([some](some.md) ([sym](sym.md) ([tree](tree.md)))))))))


<br clear=all>
<p align=center>
<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
</p>

## Start-up, test suite, demos



```lua
local all = require"all"
local chat,chunks,cli,csv = all.chat, all.chunks, all.cli, all.csv
local maps,on = all.maps, all.on
local settings,sort,splice, the = all.settings, all.sort, all.splice, all.the

local COLS,NUM = require"cols", require"num"
local SOME, SYM, NB  = require"some", require"sym", require"nb"
local ABCD,ROWS      = require"abcd", require"rows"
require "tree"
```


To disable a test, rename it from `go` to `no`.



```lua
local go,no = {},{}
```


Print `the`.



```lua
function go.THE() chat(the); return true end
```


Sort some numbers.



```lua
function go.SORT() chat(sort{10,5,1,15,0}); return true end
```


Iterate over 2 lists



```lua
function go.MAPS() 
  chat(maps({1,2,3},{10,20,30}, 
       function(x,y) return x+y end)); return true end
```


 Summarize stream of numbers



```lua
function go.NUM() 
  local n=NUM(); for i=1,1000 do n:add(i) end; chat(n)
  print(n:div())
  return true end
```


Keep a sample of 32 nums (out of 1000).



```lua
function go.SOME() 
  local s=SOME(32); for i=1,1000 do s:add(i) end
  chat(sort(s.kept)); return true end 
```


 Summarize stream of symbols



```lua
function go.SYM() 
  local s=SYM()
  for i=1,1000 do for _,c in pairs{"a","a","b"} do s:add(c) end end
  print(s:div())
  chat(sort(s.kept)); return true end 
```


Print CSV file.



```lua
function go.CSV() csv(the.file, chat); return true end
```


Try initializing some columns from a list of names.



```lua
function go.COLS() chat(COLS{"aa","Bb","Cc-"}.x); return true end
```


Load data from a csv file to a ROWS object.



```lua
function go.ROWS( rs) 
  rs=ROWS():fill(the.file)
  chat(rs.cols.x[1])
  chat(rs.cols.y); return true end
```


Print klass names



```lua
function go.KLASS() 
  local file = "../../data/diabetes.csv"
  local s=SYM()
  for _,row in pairs(ROWS():fill(file).rows) do s:add(row:klass()) end
  chat(s.kept)
  return true end
```


Load data from a csv file to a ROWS object.



```lua
function go.BETTERS( rs,best,m,rest) 
  rs=ROWS():fill(the.file)
  sort(rs.rows) 
  m    = (#rs.rows)^.5
  best = splice(rs.rows,1,m)  --(m^.5)) 
  rest = splice(rs.rows,#rs.rows - m) --#rs.rows - 30) --(m^.5)) 
  chat(rs:clone(best):mids())
  chat(rs:clone(rest):mids())
  return true end

function go.DIABETES(f,  a,n) --   i,t,a) 
  a = ABCD()
  n= NB(f or "../../data/diabetes.csv",function(got,want) a:add(got,want) end)
  a:pretty( a:report() ) 
  return true end

function go.SOYBEAN()  
  go.DIABETES("../../data/soybean.csv") 
  return true end

function go.CHUNKS()
  if the.file:find".lua$" then
    chunks(the.file); return true end
  return true end

function go.BINS( rs, m,best,rest)
  rs=ROWS():fill(the.file)
  sort(rs.rows) 
  m    = (#rs.rows)*.1
  best = splice(rs.rows,1,m)  --(m^.5)) 
  rest = splice(rs.rows,#rs.rows - m) --#rs.rows - 30) --(m^.5)) 
  rs:tree{best,rest}:branches()
  return true
end

```


### Start



```lua
the = cli(the)
on(the, go)
```


