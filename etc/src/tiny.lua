---- ---- ---- ---- Config
local the= { 
     bins  = 7,
     far   = .95,
     files = "../../data/auto93.csv",
     go    = "nothing",
     min   = .5,
     nums  = 512,
     p     = 2,
     seed  = 10019,
     some  = 512
     }
---- ---- ---- ---- Names
-- Trap prior names (for liniting, at end)
local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
-- Define this module
local m={}
-- Config
m.config={the}
-- Lib
m.lib={maths={},lists={},read={},write={}}
local         rand,rnd
m.lib.maths= {rand,rnd}
local         rev,sort,lt,gt,push,map,any,many,per
m.lib.lists= {rev,sort,lt,gt,push,map,any,many,per}
local         fmt,chat,cat
m.lib.print= {fmt,chat,cat}
local        coerce,cli,words,lines,csv,csv2data
m.lib.read= {coerce,cli,words,lines,csv,csv2data}

-- Types
local       is,COL,ROW,ABOUT,DATA
m.types=   {is,COL,ROW,ABOUT,DATA}

-- Update methods
local       add,adds,row,clone
m.update=  {add,adds,row,clone}

-- Query methods
local       has,norm,mid,div,stats,better
m.query=   {has,norm,mid,div,stats,better}

-- Distance methods
local       dist,around,far,half,halfsort
m.dist=    {dist,around,far,half,halfsort}

-- Startup
local   go={}

---- ---- ---- ---- Types
-- Our CSV files have column names on row1. `is` recognizes column types.
is={num   = "^[A-Z]",  -- numeric cols start with uppercase
    goal  = "[!+-]$",  -- !=klass, [+,-]=maximize,minimize
    klass = "!$",      -- klass if "!"
    skip  = ":$",      -- skip if ":"
    less  = "-$"}      -- minimize if "-"

-- Summarize data seen in different columns. 
function COL(num,txt)
  txt=txt or ""
  return {n    = 0,                -- how many items seen?
          at   = num or 0,         -- position ot column
          txt  = txt,              -- column header
          nump = txt:find(is.num), -- is this a number?
          w    = txt:find(is.less) and -1 or 1,
          ok   = true,             -- false if some update needed
          has = {}} end            -- place to keep (some) column values.

-- Holds one record of data
function ROW(about, t)
  return {about=about,  -- pointer to background column info 
          cells=t,      -- raw values
          cooked=t} end -- where we might store (e.g) discretized values

-- Holds `rows`, summarized in `baout`.
function DATA() return {rows={},about=nil} end

-- Factory for making columns from column header strings.
-- Goals and none-gaols are cached in `x` and `y` (ignorong
-- anything that is `skipped`.
function ABOUT(strs)
  local about = {names=strs,all={}, x={}, y={}, klass=nil}
  for at,txt in pairs(strs) do 
    local one = push(about.all, COL(at,txt))
    if not txt:find(is.skip) then
      push(txt:find(is.goal) and about.y or about.x, one)
      if txt:find(is.klass) then about.klass=one end end end 
  return about end

---- ---- ---- ---- Methods
---- ---- ---- Update
-- Add something into a `col`. For `nump` cols, keep at most 
-- `the.nums` (after which, replace old items at random). For
-- other columns, count how many times we have seen `x`.
function add(col,x)
  if x ~= "?" then
    col.n = col.n + 1
    if   not col.nump 
    then col.has[x] = 1 + (col.has[x] or 0) 
    else local pos
         if     #col.has < the.nums      then pos= (#col.has) + 1 
         elseif rand() < the.nums/self.n then pos= rand(#col.has) end
         if pos then 
           col.ok=false  -- the `kept` list is no longer in sorted order
           col.has[pos]=x end end end end

-- Add a row of data across all columns. 
function adds(about,x)
  local row = x.cells and x or ROW(about,x) -- ensure that "x" is a row.
  for _,cols in pairs{about.x,about.y} do
    for _,col in pairs(cols) do add(col, row.cells[col.at]) end end 
  return row end

-- Add a `row` to `data`. If this is top row, use
-- `t` to initial `data.about`.
function row(data,t)
  if   data.about 
  then push(data.rows, adds(data.about,t))
  else data.about = ABOUT(t) end end

-- Copy the structure of `data`. Optionally, add rows of
-- data (from `t`).
function clone(data,t)
  local data1= DATA()
  row(data1, data.about.names)
  for _,row1 in pairs(t or {}) do row(data1,row1) end
  return data1 end

---- ---- ---- Query
-- Return `col.has`, sorting numerics (if needed).
function has(col)
  if col.nump and not col.ok then table.sort(col.has); col.ok=true end
  return col.has end

-- Return `num`, normalized to 0..1 for min..max.
function norm(col,num)
  local a= has(col) -- "a" contains all our numbers,  sorted.
  return a[#a] - a[1] < 1E-9 and 0 or (num-a[1])/(a[#a]-a[1]) end

-- Return the central tendency of `col`umns (median/mode for
-- numerics/other (respectively).
function mid(col,places)
  if col.nump then 
    local median= per(has(col),.5)  
    return places and rnd(median,places) or median end -- for numerics
  local mode,most= -1,nil
  for x,n in pairs(col.has) do if n>most then mode,most=x,n end end
  return mode end -- mode for symbols

-- Return the diversity of a `col`umns (sd/entropy for
-- numerics/other (respectively).
function div(col,places)
  if col.nump then 
    local sd = (per(has(col),.9) - per(has(col),.1))/2.58 
    return places and rnd(sd,places) or sd end
  local ent=0
  for _,n in pairs(col.has) do 
    if n>0 then ent=ent-n/col.n*math.log(n/col.n,2) end end
  return places and rnd(ent,places) or ent end 

-- Returns stats collected across a set of `col`umns (stats
-- selected by `f`). If `places` omitted, then no nums are rounded.
-- If `cols` is omitted then report the `y` values.
function stats(data,   f,places,cols,   u)
  f =  f or mid
  cols =cols or data.about.y
  u={}; for k,col in pairs(cols) do 
    u.n=col.n; u[col.txt]=f(col,places) end; 
  return u end

-- Return true if `row1`'s goals are better than `row2`.
function better(row1,row2)
  local s1,s2,d,n=0,0,0,0
  local ys,e = row1.about.y,math.exp(1)
  for _,col in pairs(ys) do
    x,y= row1.cells[col.at], row2.cells[col.at]
    x,y= norm(col,x), norm(col,y)
    s1 = s1 - e^(col.w * (x-y)/#ys)
    s2 = s2 - e^(col.w * (y-x)/#ys) end
  return s1/#ys < s2/#ys end 

---- ---- ---- Dist
-- Return 0..1 for distance between two rows using `cols`
-- (and `cols`` defaults to the `x` columns).
function dist(row1,row2,cols)
  local d,n,x,y,dist1=0,0
  function dist1(col,x,y)    
    if x=="?" and y=="?" then return 1 end
    if   col.nump 
    then if     x=="?" then y=norm(col,y); x=y<.5 and 1 or 0 
         elseif y=="?" then x=norm(col,x); y=x<.5 and 1 or 0 
         else   x,y = norm(col,x), norm(col,y) end
         return math.abs(x-y) 
    else return (x=="?" or y=="?") and 1 or x==y and 0 or 1 end 
  end ---------------
  cols = cols or row1.about.x
  for _,col in pairs(cols) do
    x,y = row1.cells[col.at], row2.cells[col.at]
    d = d+dist1(col,x,y)^the.p
    n = n + 1 end
  return (d/n)^(1/the.p) end

-- Return all rows  sorted by their distance  to `row`.
function around(row1,rows)
  return sort(map(rows, function(row2) return {row=row2,d=dist(row1,row2)} end),
             lt"d") end

-- Find two distant rows, then divide data according to its
-- distance to those two rows. To reduce the cost of this search,
-- only apply it to `some` of the rows (controlled by `the.some`).
-- If `rowAbove` is supplied,
-- then use that for one of the two distant items. 
function half(rows,  rowAbove)
  local As,Bs,A,B,c,far,project = {},{}
  local some= many(rows,the.some)
  function far(row)     return per(around(row,some), the.far).row end
  function project(row) return {row=row,
                                x=(dist(row,A)^2 + c^2 - dist(row,B)^2)/(2*c)} end
  A= rowAbove or far(any(some))
  B= far(A)
  c= dist(A,B) 
  for n,rd in pairs(sort(map(rows, project),lt"x")) do 
    push(n < #rows/2 and As or Bs, rd.row) end
  return A,B,As,Bs,c end

-- Divide the data, accumulate the worst half, recurse on the rest.
-- Return the shriving best and the worst. Each returned list is
-- sorted L to R, best to less.
function halfsort(rows,rowAbove,  stop,worst)
  stop = stop or (#rows)^the.min
  worst = worst or {}
  if   #rows < stop 
  then return rows,worst -- rows is shriving best
  else local A,B,As,Bs = half(rows,rowAbove)
       if better(A,B) then 
         for _,row in pairs(reverse(Bs)) do push(worst,row) end
         return halfsort(reverse(As),A,stop,worst)
       else
         for _,row in pairs(As) do push(worst,row) end
         return halfsort(Bs,B,stop,worst) end end  end
     
---- ---- ---- ---- Library
---- ---- ---- Maths
-- Random num
rand=math.random

-- Round nums.
function rnd(num, places)  
  local mult = 10^(places or 3)
  return math.floor(num * mult + 0.5) / mult end

---- ---- ---- Lists
-- In-place reverse, return reversed list
function rev(t)
  for i=1, math.floor(#t / 2) do t[i],t[#t-i+1] = t[#t-i+1],t[i] end
  return t end

-- In-place sort,  returns sorted list
function sort(t,f) table.sort(t,f); return t end
-- Sorting predictates
function lt(x) return function(a,b) return a[x] < b[x] end end
function gt(x) return function(a,b) return a[x] > b[x] end end

-- Add `x` to list `t`, returning `x`.
function push(t,x) t[1+#t]=x; return x end 

-- Return items in `t` filtered through `f`. If `f` ever returns nil
-- then the returned list will be shorter.
function map(t,f) 
  local u={}; for _,v in pairs(t) do u[1+#u]=f(v) end; return u end

-- Return any item (selected at random) from list `t`.
function any(t) return t[rand(#t)] end

-- Return `num` items (selected at random) from list `t`.
-- If `num` is more than the size of the list, return that list, shuffled.
function many(t,num, u) 
  if num>#t then return shuffle(t) end
  u={}; for j=1,num do u[1+#u]= any(t) end; return u end

-- Return the `p`-th item in `t` (assumed to be sorted). e.g.
-- `per(t,.5)` returns the median.
function per(t,p) 
  p=math.floor((p*#t)+.5); return t[math.max(1,math.min(#t,p))] end

---- ---- ---- Print
-- Emulate Printf
fmt = string.format

-- Generate a string from `t` and print it (returning `t`).
function chat(t) print(cat(t)) return t end 
-- Generate a string from `t`.
function cat(t,   show,u)  
  if type(t)~="table" then return tostring(t) end
  function show(k,v) return #t==0 and fmt(":%s %s",k,v) or tostring(v) end
  u={}; for k,v in pairs(t) do u[1+#u]=show(k,v) end
  return (t._is or "").."{"..table.concat(#t==0 and sort(u) or u," ").."}" end

---- ---- ---- Read
-- Try reading `str` as a boolean, then int, then float, then string.
function coerce(str)
  str = str:match"^%s*(.-)%s*$" 
  if str=="true" then return true elseif str=="false" then return false 
  else return math.tointeger(str) or tonumber(str) or str end  end

-- Read update for `slot` of table from command line flag `-s` or `--slot`.
-- If slot's is a boolean, this code flips old value.
function cli(t)
  for slot,v in pairs(t) do
    v = tostring(v)
    for n,x in ipairs(arg) do 
      if x=="-"..(slot:sub(1,1)) or x=="--"..slot then
        v = v=="false" and "true" or v=="true" and "false" or arg[n+1] end end
    t[slot] =  coerce(v) end
  return t end

-- Split  `str` on `sepstr`, filter each part through `fun`, return the resulting list.
function words(str,sepstr,fun,      t)
  fun = fun or function(z) return z end
  sepstr = fmt("([^%s]+)",sepstr)
  t={};for x in str:gmatch(sepstr) do t[1+#t]=fun(x) end;return t end 

-- Read lines from `filestr`, closing stream at end. Call `fun` on each line.
function lines(filestr, fun)
  local src = io.input(filestr)
  while true do
    local str = io.read()
    if not str then return io.close(src) else fun(str) end end end 

-- Read lines from `filestr`, converting each into words, passing that to `fun`.
function csv(filestr, fun)
  lines(filestr, 
    function(t) fun(words(t,",",coerce)) end) end

-- Read `filestr` into a DATA object. Return that object.
function csv2data(filestr,data)
  data=DATA()
  csv(filestr, function(t) row(data,t) end) 
  return data end

---- ---- ---- ---- Tests
-- Tests fail if they do not return `true`.
function go.the() chat(the); return true end

function go.one(data1,data2)
  data1=csv2data("../../data/auto93.csv")
  print("mid1", cat(stats(data1,mid,2))) 
  print("div1", cat(stats(data1,div,2)))
  data2=clone(data1,data1.rows) 
  print("mid2", cat(stats(data2,mid,2))) 
  print("div2", cat(stats(data2,div,2)))
  return true
  end

function go.dist(data,row1,row2)
  data= csv2data("../../data/auto93.csv")
  chat(data)
  print(#data.rows)
  for i = 1,20 do
    row1=any(data.rows)
    row2=any(data.rows)
    print(dist(row1,row2)) end 
  return true end

---- ---- ---- ---- Start-up
-- Counter for test failures
local fails=0

-- Run one test. Beforehand, reset random number seed. Afterwards,
-- reset the settings to whatever they were before the test.
local function run(str)
  if type(go[str])=="function" then
    local saved={};for k,v in pairs(the) do saved[k]=v end
    math.randomseed(the.seed)
    if true ~= go[str]() then fails=fails+1; print("FAIL",str) end
    for k,v in pairs(saved) do the[k]=v end  end end 

-- If this code is being loaded via a `require` statement,
-- just return the names. 
if pcall(debug.getlocal,4,1) then
  return {config=config,types=types,update=update,
          query=query,dist=dist,lib=lib}
else
   -- Else, update the settings from command line.
   the = cli(the)
   -- Run the tests.
   local todo ={}; for k,_ in pairs(go) do push(todo,k) end
   for _,k in pairs(the.go=="all" and sort(todo) or {the.go}) do run(k) end
   -- Check for any rogue local variables.
   for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end
   -- Report back to the operating system how many failures were seen.
   os.exit(fails)
end
