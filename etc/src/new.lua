local _=require"lib"
local the=require"the"
local obj,push = _.obj,_.push

--> SYM(at:?int, txt:?str) :SYM --> Summarize a stream of non-numerics.
local SYM=obj("SYM", function(i,at,txt)
  i.at, i.txt, i.n, i.kept =  at or 0, txt or "", 0, {}

--> NUM(at:?int, txt:?str) :NUM --> Summarize a stream of numbers.
local NUM=obj("NUM", function(i,at,txt) 
  i.at, i.txt, i.n, i.kept =  at or 0, txt or "", 0, {}
  i.nums, i.ok, i.w = 256, true, i.txt:find"-$"  end)

--> COLS(names:[str]) :COLS --> Factory. Turns a list of names into NUMs or SYMs.
-- Goal columns get added to `i.y` and others to `i.x (unless denoted `ignored`). 
-- A klass column gots to `i.klass`.
local COLS = obj("COLS", function(i,names) 
  i.names, i.x, i.y, i.all,i.klass, i.names = names, {}, {},  {}
  for at,txt in pairs(names) do
    col = (name:find"^[A-Z]" and NUM or SYM)(at,txt) end
    push(i.all, col)
    if not col.txt:find":$" then
      push(col.txt:find"[!+-]$" and i.y or i.x, col)
      if col.txt:find"!$" then i.klass=col end end end ) 

--> ROW(of:ROWS, cells:tab) :ROW --> Place to store one record
-- (and stats on how it is used; e.g. `i.evaled=true` if we touch the y values.
local ROW=obj("ROW", function(i,of,cells) 
  i._of,i.cells,i.evaled = of,cells,false end)

--> ROWS(names:[str]) :ROWS --> Place to store many ROWs
--  and summarize them (in `i.cols`).
local ROWs=obj("Rows", function(i,names) i.rows={}; i.cols=COLS(names) end)

return {SYM=SYM, NUM=NUM, COLS=COLS, ROW=ROW, ROWS=ROWS}
