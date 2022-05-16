local the = {bins=16, file="../etc/data/auto93.csv"}
local big = 1E32
local fmt = string.format

local function push(t,x) t[1+#t]=x; return x end
local function sort(t,f) table.sort(t,f); return t end
local function map(t,f, u)
  u={};for k,v in pairs(t) do u[1+#u]=f(v) end; return u end

local function string2thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x  end

local function csv(csvfile) 
  csvfile = io.input(csvfile)
  return function(line, row) 
    line=io.read()
    if not line then io.close(csvfile) else
      row={}; for x in line:gmatch("([^,]+)") do push(row,string2thing(x)) end
      return row end end end 

local function o(t,    u)
  if #t>0 then return "{"..table.concat(map(t,tostring)," ").."}" else
    u={}; for k,v in pairs(t) do u[1+#u] = fmt(":%s %s",k,v) end
    return (t.is or "").."{"..table.concat(sort(u)," ").."}" end end
local function oo(t) print(o(t)) end

local function cli(d)
  for slot,x in pairs(d) do
    x = tostring(x)
    for n,flag in ipairs(arg) do 
      if flag=="--"..slot or flag=="-"..slot:sub(1,1) then
        x = x=="false" and"true" or x=="true" and"false" or arg[n+1] end end 
    d[slot] = string2thing(x) end 
  return d end

the = cli(the)

--------------------------------------------------------------------------------
local function is(name,    t,new)
  function new(kl,...) local x=setmetatable({},kl); kl.new(x,...); return x end 
  t = {__tostring=o, is=name or ""}; t.__index=t
  return setmetatable(t, {__call=new}) end

local SYM,NUM,COLS,ROW,ROWS = is"SYM", is"NUM", is"COLS", is"ROW", is"ROWS"

--------------------------------------------------------------------------------
local function usable(x) return x~="?" end
local function skip(x)   return x:find":$" end
local function nump(x)   return x:find"^[A-Z]" end
local function goalp(x)  return x:find"[!-+]$" end
local function klassp(x) return x:find"!$" end

function SYM.new(i,at,txt) i.at,i.txt=at,txt; i.all = {} end
function SYM.add(i,x)      if usable(x) then i.all[x]= 1+(i.all[x] or 0) end end 
function SYM.range(i,x,n)  return x end

function NUM.new(i,at,txt) i.at,i.txt=at,txt; i.lo=big; i.hi= -big end
function NUM.add(i,x) 
  if usable(x) then i.lo,i.hi = math.min(i.lo,x), math.max(i.hi,x) end end
function NUM.range(i,x,n,  b) b=(i.hi-i.lo)/n; return math.floor(x/b+0.5)*b end

function ROW.new(i,cells,egs) i.cells=cells; i.egs=egs end
function ROW.lt(i,j,n)
  i,j = i[n], j[n]
  i   = type(i)~="number" and -1E32 or i
  j   = type(i)~="number" and -1E32 or j
  return i < j end

function COLS.new(i,t)
  i.names, i.all, i.x, i.y, i.nums = t,{},{},{},{}
  for at,name in pairs(i.names) do 
    col = push(i.all, (nump(name) and NUM or SYM)(at,name))
    if not skip(name) then
      push(goalp(name) and i.y or i.x, col)
      if klassp(name) then i.klass = col end end end end
function COLS.add(i,cells)
  for at,col in pairs(i.all) do col:add(cells[at]) end end

function ROWS.new(i,src)
  i.rows, i.cols = {},nil
  if   type(src)=="table" 
  then for _,row in pairs(src) do i:add(row) end 
  else for   row in csv(src)   do i:add(row) end end end
function ROWS.add(i,x)
  if   i.cols 
  then i.cols:add(push(i.rows, x.cells and x or ROW(x,i)).cells)
  else i.cols = COLS(x) end end

function camel(    i)
  i=ROWS(the.file) 
end
