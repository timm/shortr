-- ## Start-up, test suite, demos
local all = require"all"
local any,cat,chat,chunks,cli,csv = all.any, all.cat,all.chat, all.chunks, all.cli, all.csv
local fmt,many,map,maps,on,rnd = all.fmt, all.many, all.map,all.maps, all.on,all.rnd
local settings,sort,splice, small,sum,the = all.settings, all.sort, all.splice, all.small, all.sum,all.the

local COLS,NUM = require"cols", require"num"
local SOME, SYM, NB  = require"some", require"sym", require"nb"
local ABCD,ROWS      = require"abcd", require"rows"
require "tree"

-- To disable a test, rename it from `go` to `no`.
local go,no = {},{}

-- Print `the`.
function go.THE() chat(the); return true end

-- Sort some numbers.
function go.SORT() chat(sort{10,5,1,15,0}); return true end

-- Iterate over 2 lists
function go.MAPS() 
  chat(maps({1,2,3},{10,20,30}, 
       function(x,y) return x+y end)); return true end

--  Summarize stream of numbers
function go.NUM() 
  local n=NUM(); for i=1,1000 do n:add(i) end; chat(n)
  print(n:div())
  return true end

-- Keep a sample of 32 nums (out of 1000).
function go.SOME() 
  local s=SOME(32); for i=1,1000 do s:add(i) end
  chat(sort(s.kept)); return true end 

--  Summarize stream of symbols
function go.SYM() 
  local s=SYM()
  for i=1,1000 do for _,c in pairs{"a","a","b"} do s:add(c) end end
  print(s:div())
  chat(sort(s.kept)); return true end 

-- Print CSV file.
function go.CSV() csv(the.file, chat); return true end

-- Try initializing some columns from a list of names.
function go.COLS() chat(COLS{"aa","Bb","Cc-"}.x); return true end

-- Load data from a csv file to a ROWS object.
function go.ROWS( rs) 
  rs=ROWS():fill(the.file)
  chat(rs.cols.x[1])
  chat(rs.cols.y); return true end

-- Print klass names
function go.KLASS() 
  local file = "../../data/diabetes.csv"
  local s=SYM()
  for _,row in pairs(ROWS():fill(file).rows) do s:add(row:klass()) end
  chat(s.kept)
  return true end

-- Load data from a csv file to a ROWS object.
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

function go.DIST(rs)
  rs=ROWS():fill(the.file) 
  for j=1,20 do 
    local x=any(rs.rows)
    local y=x:far()
    print(j,fmt("%4.4f",x-y), cat(x.cells), cat(y.cells)) end 
  return true end

function go.HALF(rs,xy)
  rs=ROWS():fill(the.file) 
  xy=rs:half()
  chat(rs:clone(xy.xs):mids())
  chat(rs:clone(xy.ys):mids())
  print(xy.y < xy.x)
  return true end

local function _calc(c,p) return math.log(math.log(1-c)/math.log(1-p),2) end
function go.BEST(rs,xy,bests,rests,best,n)
  rs=ROWS():fill(the.file) 
  rows = rs.rows
  local n1,n2=20,20
  bests1,rests1=rs:best(rows,n1) 
  print("bests1",cat(rs:clone(bests1):mids()))
  print("rests1",cat(rs:clone(rests1):mids()))
  n=sum(rs.rows,function(row) return row.evaled and 1 or 0 end); print("eval1",n)

  bests2,rests2=rs:best(bests1,n2)
  print("bests2",cat(rs:clone(bests2):mids()))
  print("rests2",cat(rs:clone(rests2):mids()))
  n=sum(rs.rows,function(row) return row.evaled and 1 or 0 end); print("eval1",n)

  for j,row in pairs(sort(rs.rows)) do row.rank=100*j/#rs.rows//1 end
  print("bests1",cat(sort(map(bests1,function(r) return r.rank end))))
  print("bests2",cat(sort(map(bests2,function(r) return r.rank end))))
  print("rand", cat(sort(map(many(rs.rows,n),function(r) return r.rank end))))

  print("\ntree")
  rows1=bests2
  rows2=splice(rests1,#rests1-3*#rows1)
  print("size",#rows1,#rows2)
  rs:tree{rows1,rows2}:branches()

  print(rnd(_calc(.99,.05),1))
  return true end
   
-- ### Start
the = cli(the)
on(the, go)
