-- ## Test suite
local all = require"all"
local chat,chunks,cli,csv = all.chat, all.chunks, all.cli, all.csv
local maps,on = all.maps, all.on
local settings,sort,splice, the = all.settings, all.sort, all.splice, all.the

local COLS,NUM, ROWS = require"cols", require"num", require"rows"
local SOME, SYM, NB  = require"some", require"sym", require"nb"
local ABCD,TREE      = require"abcd", require"tree"

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
function go.NUMS() 
  local n=NUM(); for i=1,1000 do n:add(i) end; chat(n)
  return true end

-- Keep a sample of 32 nums (out of 1000).
function go.SOME() 
  local s=SOME(32); for i=1,1000 do s:add(i) end
  chat(sort(s.kept)); return true end 

--  Summarize stream of symbols
function go.SYM() 
  local s=SYM()
  for i=1,1000 do for _,c in pairs{"a","a","b"} do s:add(c) end end
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
  rest = splice(rs.rows,1,#rs.rows - m) --#rs.rows - 30) --(m^.5)) 
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
    chunks(the.file); return true end end
-------
-- ### Start
the = cli(the)
on(the, go)
