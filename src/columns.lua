-- vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro 
--------- --------- --------- --------- --------- --------- 

-- This is a helper class for [Tbl](tbl.html). Everything that tables
-- knows about its columns, are stored here in `Columns`.
--
-- (By the way, if this code seems a little arcane, just relax. Its
-- only called once on the first row. )
--

local Object = require("object")
local Num    = require("num")
local Sym    = require("sym")
local Columns   = {is="Columns"}

function Columns.new()
  local i = Object.new()
  i.me    = Columns
  i.all, i.nums, i.syms = {},{},{}
  i.x ={all={},  nums={}, syms={}}
  i.y ={all={},  nums={}, syms={}, goals={}, klass=nil}
  i.names = {}
  return i
end

-- ----------------------------------
-- When we read a csv file, the column names
-- on the first row can contain certain magic characters
-- that tell us the type of the column:

local function usep(x)   return not x:match("%?") end
local function goalp(x)  return x:match("[<>]") end
local function klassp(x) return x:match("!") end
local function depp(x)   return klassp(x) or goalp(x) end
local function nump(x)   return goalp(x) or x:match("%$") end

-- For example, here some data with a goal of
-- reducing temperature (denoted `<temp`) while predicting
-- for `!play` (and `$humid` is a number). Note that
-- `?wind` means we intend to ignore that column. Also,
-- when something has no magic (e.g. `outlook`) then it is just
-- an independent symbol:
--
--       outlook,  <temp, $humid, ?wind, !play
--       sunny, 85, 85,  FALSE, no
--       sunny, 80, 90, TRUE, no
--       overcast, 83, 86, FALSE, yes
--       rainy, 70, 96, FALSE, yes
--       rainy, 68, 80, FALSE, yes # comments
--       rainy, 65, 70, TRUE, no
--       overcast, 64, 65, TRUE, yes
--       sunny, 72, 95, FALSE, no
--       sunny, 69, 70, FALSE, yes
--       

-- With this little language for the column headers,
-- if we
-- wanted to make a whole new table with the
-- same structure as some current one, 
-- we just pass along the `i.names` we used to make the
-- old table.

function Columns.clone(i)
  return Columns.add(Columns.new(), i.names)
end

-- ----------------------------------
-- The actual work of reading the names and placing them in
-- right list is done by `Col.add`.
-- Using the  column names, when we create a `Num` or a `Sym`
-- (to store what we find in each column) then we add that new
-- `Column` into one or more lists:
--
-- There are many types of columns so `Col.add` needs to peek at the names
-- for the magic characters.
-- Some columns, marked with a `?`, we just want to skip over.
-- Also,  some columns are numeric `nums`, marked with a `$`, and all
--   the rest are symbolic.
--
--  Some columns are numeric `goals` we want to minimize or maximize.
--  These are marked with `<` or `>` for _minimize_ or _maximize_.
--  Other columns, marked with a `!` are `klass`es whose value 
--   we want to predict.
--
-- All goal and klass columns are dependent `y` columns,
--   and anything else is an independent `x` column.
--
--

function Columns.add(i,cells,   what,alike,xs,ys,new)
  local function add(a) a[#a+1] = new end
  i.names = cells
  for c,x in pairs(cells) do
    if   nump(x)
    then what, alike,xs,ys = Num, i.nums, i.x.nums, i.y.nums
    else what, alike,xs,ys = Sym, i.syms, i.x.syms, i.y.syms
    end
    new = what.new{pos=c,txt=x}
    add(i.all)
    add(alike)
    add(depp(x) and ys or xs)
    add(depp(x) and i.y.all or i.x.all)
    if goalp(x)  then add(i.y.goals) end 
    if klassp(x) then i.y.klass = new end end 
  return i
end

-- ----------
-- And finally...



return Columns
