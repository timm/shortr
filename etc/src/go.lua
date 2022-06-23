-----
-- Test suite.
local _ = require"about"
local chat,cli,csv,maps,on = _.chat, _.cli, _.csv,  _.maps, _.on
local settings,sort,the    = _.settings, _.sort, _.the

local klass=require"obj"
local COLS,NUM, ROWS = require"COLS", require"NUM", require"ROWS"
local SOME, SYM      = require"SOME", require"SYM"

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
  local n=NUM(); for i=1,1000 do n:summarize(i) end; chat(n)
  return true end

-- Keep a sample of 32 nums (out of 1000).
function go.SOME() 
  local s=SOME(32); for i=1,1000 do s:summarize(i) end
  chat(sort(s.kept)); return true end 

--  Summarize stream of symbols
function go.SYM() 
  local s=SYM()
  for i=1,1000 do for _,c in pairs{"a","a","b"} do s:summarize(c) end end
  chat(sort(s.kept)); return true end 

-- Print CSV file.
function go.CSV() csv(the.file, chat); return true end

-- Try initializing some columns from a list of names.
function go.COLS() chat(COLS{"aa","Bb","Cc-"}.x); return true end

-- Load data from a csv file to a ROWS object.
function go.ROWS() 
  chat(ROWS():fill(the.file).cols.y); return true end

-- Print klass names
function go.KLASS() 
  local file = "../../data/diabetes.csv"
  local s=SYM()
  for _,row in pairs(ROWS():fill(file).rows) do s:summarize(row:klass()) end
  chat(s.kept)
  return true end

-------
-- ### Start
the = cli(the)
on(the, go)
