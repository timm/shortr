-- ## Define demos / tests

local _ = require"about"
local chat,cli,csv,goes,maps = _.chat, _.cli, _.csv, _.goes, _.maps
local settings,sort,the      = _.settings, _.sort,_.the
local klass=require"klass"
local COLS,NUM, ROWS = klass.COLS,klass.NUM, klass.ROWS
local SOME, SYM      = klass.SOME, klass.SYM
require"add"
require"query"

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
function go.ROWS() chat(ROWS():adds(the.file).cols.y); return true end

-------
-- ### Start
goes(the, go)
