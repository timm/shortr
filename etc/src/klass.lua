-- ## Define classes    
local _=require"about"
local obj,push,the = _.obj,_.push,_.the

--> SYM(at:?int, txt:?str) :SYM -> Summarize a stream of non-numerics.
local SYM = obj("SYM", function(i,at,txt)
  i.at, i.txt, i.n, i.kept =  at or 0, txt or "", 0, {} end)

--> SOME(max:?int) :SOME -> collect, at most, `some` numbers.
local SOME = obj("SOME", function(i,max) 
  i.kept, i.ok, i.max, i.n = {}, true, max, 0  end)

--> NUM(at:?int, txt:?str) :NUM -> Summarize a stream of numbers.
local NUM = obj("NUM", function(i,at,txt) 
  i.at, i.txt, i.n, i.kept =  at or 0, txt or "", 0, SOME(the.Some)
  i.ok, i.w =  true, i.txt:find"-$"  end)

--> COLS(names:[str]) :COLS -> Factory. Turns a list of names into NUMs or SYMs.
-- Goal columns get added to `i.y` and others to `i.x (unless denoted `ignored`). 
-- A klass column goes to `i.klass`.
local COLS = obj("COLS", function(i,names) 
  i.names, i.x, i.y, i.all,i.klass, i.names = names, {}, {},  {}
  for at,txt in pairs(names) do
    local col = (txt:find"^[A-Z]" and NUM or SYM)(at,txt) 
    push(i.all, col)
    if not col.txt:find":$" then
      push(col.txt:find"[!+-]$" and i.y or i.x, col)
      if col.txt:find"!$" then i.klass=col end end end end ) 

--> ROW(of:ROWS, cells:tab) :ROW -> Place to store one record
-- (and stats on how it is used; e.g. `i.evaled=true` if we touch the y values.
local ROW = obj("ROW", function(i,of,cells) 
  i._of,i.cells,i.evaled = of,cells,false end)

--> ROWS(names:?[str], rows:?[ROW}) :ROWS -> Place to store many ROWS
--  and summarize them (in `i.cols`).
local ROWS = obj("ROWS", function(i,names,rows) 
  i.overall,i.cols,i.rows = nil,nil,{}
  if names then i.cols = COLS(names) end 
  for _,row in pairs(rows or {}) do i:add(row) end
  end)

--> ROWS.clone(init:?[ROW]) :ROWS -> Return a ROWS with same structure as `i`. 
-- Optionally, `init`ialize it with some rows.
function ROWS.clone(i,init)
  local j=ROWS(i.cols.names,init)
  j.overall = i.overall or i
  return j end

-----
-- ### Return
return {SOME=SOME, SYM=SYM, NUM=NUM, COLS=COLS, ROW=ROW, ROWS=ROWS}
