-- vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro 
--------- --------- --------- --------- --------- --------- 

-- <img align=right  width=200
--  src="https://github.com/timm/lua/raw/master/etc/img/cols.jpg">
--
-- This is a helper class for [Tbl](tbl.html). Everything that tables
-- knows about its columns, are stored here in `Cols`.
--
-- (By the way, if this code seems a little complex, just relax. Its
-- only called once on the first row. )
--

local Object = require("object")
local Num    = require("num")
local Sym    = require("sym")
local Cols   = {is="Cols"}

function Cols.new()
  local i = Object.new()
  i.me    = Cols
  i.all, i.nums, i.syms = {},{},{}
  i.x ={all={},  nums={}, syms={}}
  i.y ={all={},  nums={}, syms={}, goals={}, klass=nil}
  i.names = {}
  i.use   = {}
  return i
end

-- When we read a csv file, the column names
-- on the first row can contain certain magic charcters
-- that tell us the type of the column:

local function usep(x)   return not x:match("%?") end
local function goalp(x)  return x:match("[<>]") end
local function klassp(x) return x:match("!") end
local function depp(x)   return klassp(x) or goalp(x) end
local function nump(x)   return goalp(x) or x:match("%$") end

-- This means that if we
-- wanted to make a whole new table with the
-- same structure as some current one, 
-- we just pass along the `i.names` we used to make the
-- old table.

function Cols.clone(i)
  return Cols.add(Cols.new(), i.names)
end



-- There are many types of columns.
-- For example, Some columns, marked with a `?`, we just want to skip over.
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
-- The actual work of reading the names and placing them in
-- right list is done by `Col.add`.
-- Using the  column names, when we create a `Num` or a `Sym`
-- (to store what we find in each column) then we add that new
-- `Column` into one or more lists:
--

function Cols.add(i,cells,   c,what,alike,xs,ys,new)
  local function push(a,x) a[#a+1] = x end
  c = 0
  for c0,x in pairs(cells) do
    if usep(x) then
      c = c+1
      i.use[c]   = c0
      i.names[c] = x
      if   nump(x)
      then what, alike,xs,ys = Num, i.nums, i.x.nums, i.y.nums
      else what, alike,xs,ys = Sym, i.syms, i.x.syms, i.y.syms
      end
      new  = what.new{pos=c,txt=x}
      push(i.all, new)
      push(alike, new)
      push(depp(x) and ys or xs, new)
      if klassp(x) then i.y.klass = new end
      if goalp(x)  then push(i.y.goals, new) end 
    end
  end 
  return i
end


return Cols
