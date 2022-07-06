-- ##  Class COLS
-- Factor for making NUMs or SYMs

-- **RESPONSIBILITIES** : 
-- - [Create](#create) columns, keeping pointers to the dependent and independent  columns in the `y` and `x` variables.
-- - [Update](#update) cukumn summaries
-- - Knows if we want to minimize or maximize these values (see `w`).

-- **COLLABORATORS** :
-- - [NUM](num.md) , [SYM](sym.md)
-- ------------------------------------------------------------
local all=require"all"
local obj, push = all.obj, all.push
local NUM, SYM = require"NUM", require"SYM"

-- ### Create
-- > COLS(names:[str]) :COLS > Factory. Turns a list of names into NUMs or SYMs. <
-- Goal columns get added to `i.y` and others to `i.x` (unless denoted `ignored`). 
-- A klass column goes to `i.klass`.
local COLS = obj("COLS", function(i,names) 
  i.names = names   -- :[str]       list of known columns names
  i.all   = {}      -- :[NUM|SYM]   all the columns
  i.x     = {}      -- :[NUM|SYM]   list of pointers to just the independent columns
  i.y     = {}      -- :[NUM|SYM]   list of pointers to just the dependent columns
  i.klass = nil     -- :?(NUM|SYM)  pointer to the klass column, may be nil.
  for at,txt in pairs(names) do i:_make1Column(at,txt) end end)

-- > _make1Column(i:COLS, at:num, txt: str) > Make NUM or Sym columns. <
-- All the columns are stored in `i.all`. Dependent and independent columns
-- are also held in `i.y` and `i.x` (respectively). Anything we are skipping
-- will make it into `all`, but not `x,y`. Ditto for any klass column (which,
-- if not empty, will be stored in `klass`.
function COLS._make1Column(i,at,txt)
  local skipp=  function(x) return (x or ""):find":$"     end -- what to ignore
  local klassp= function(x) return (x or ""):find"!$"     end -- single goal
  local goalp=  function(x) return (x or ""):find"[!+-]$" end -- dependent column
  local nump=   function(x) return (x or ""):find"^[A-Z]" end -- NUM or SYM?
  local col =   (nump(txt) and NUM or SYM)(at,txt) 
  push(i.all, col)
  if not skipp(txt) then
    push(goalp(txt) and i.y or i.x, col)
    if klassp(txt) then i.klass = col end end end 

-- ### Update
-- > add(i:COLS: row:ROW) > Update columns using data from `row`. <
-- This code only updates the `x,y` columns (so we do not take up space
-- collecting data on "skipped" columns). 
function COLS.add(i,row)
  for _,cols in pairs{i.x,i.y} do
    for _,col in pairs(cols) do col:add(row.cells[col.at]) end end end

-- That's all folks
return COLS
