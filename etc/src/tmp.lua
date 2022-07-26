local the,help={},[[

TAR3: recursively find grow, combine, useful rangess

OPTIONS:
 -b  --bins  number of bins                           = 16
 -e  --era   process data in "era"s of size, say, 256 = 256
 -h  --help  show help                                = false

Boolean flags need no arguments e.g. "-h" sets "help" to "true".   ]]

-- ## Names


local cat,chat,cli,coerce,csv,eras,fmt
local push,obj

function obj(txt,fun, i)
  local function new(k,...) i=setmetatable({},k); fun(i,...); return i end
  local t={__tostring = function(x) return txt..cat(x) end}
  t.__index = t;return setmetatable(t,{__call=new}) end

-- ### Categories


-- #### Columns


-- Summarize stream of symbols
local SYM=obj("SYM",function(self, at,txt) 
  self.n   = 0          -- number of items seen
  self.at  = at or 0    -- column number
  self.txt = txt or ""  -- column name
  self.kept= {}   end)   -- counters for symbols

--**NUM(?num=0, ?str="")**<br>Summarize streams of numbers in ROWs
local NUM=obj("NUM",function(self, at,txt)
  self.n   = 0                        -- number of items seen
  self.at  = at   or 0                -- column number
  txt=txt or ""
  self.txt = txt                      -- column name
  self.w   = txt:find"-$" and -1 or 1 -- If minimizing, then -1. Else 1
  self.kept= {}                       -- some sample of the seen items
  self.ok  = false end )              -- true if sorted, set to false by each add

-- #### ROWS


local COLS=obj("COLS", function(self,names)
  self.names= names -- list of column names
  self.all  = {}    -- [NUM|SYM] all names, converted to NUMs or SYMs
  self.x    = {}    -- [NUM|SYM] just the independent columns
  self.y    = {}    -- [NUM|SYM] just the dependent columns
  self.klass= nil   -- SYM       the klass column (if it exists)
  for at,txt in pairs(names) do
    local col= push(self.all, (txt:find"^[A-Z]" and NUM or SYM)(at,txt))
    if not txt:find":$" then
      if txt:find"!$" then self.klass = col end
      push(txt:find"[!+-]$" and self.y or self.x, col) end end end)

local ROWS=obj("ROWS", function(self,file)
  self.rows={}
  self.cols=nil
  csv(file, function(row)
     if self.cols then self.cols:add(push(self.rows,row)) 
                  else self.cols=COLS(row) end end) end) 

-- ## learner


function COLS:__add(row)
  for _,cols in pairs{self.x,self.y} do
    for col in pairs(cols) do  col:add(row.raw[col.at]) end end end

-- ## Lib


-- ### Lists


function push(t,x) t[1+#t]=x; return x end
-- ### Thing to string


fmt=string.format

function cat(t)
  if type(t)~="table" then return tostring(t) end
  local function show(k,v) return #t==0 and fmt(":%s %s",k,v) or tostring(v) end
  local u={}; for k,v in pairs(t) do u[1+#u]=show(k,v) end
  if #t==0 then table.sort(u) end
  return (t._is or "").."{"..table.concat(u," ").."}" end

function chat(t) print(cat(t)) return t end

-- ### String to thing


function cli(t,helps)
  for k,v in pairs(t) do
    v = tostring(v)
    for n,x in ipairs(arg) do if x=="-"..(k:sub(1,1)) or x=="--"..k then
      v = v=="false" and "true" or v=="true" and "false" or arg[n+1] end end
    t[k] =  coerce(v) end
  if t.help then os.exit(print(helps)) end
  return t end

function coerce(x)
  local function other(z)
    if     z=="true"  then return true
    elseif z=="false" then return false
    else   return z end  end
  return math.tointeger(x) or tonumber(x) or other(x:match"^%s*(.-)%s*$") end

function csv(src,fun,c)
  local sep, lines,words
  sep=fmt("([^%s]+)",c or ",")
  function lines(file, fun1)
    local stream = io.input(file)
    while true do
      local line = io.read()
      if not line then return io.close(stream) else fun1(line) end end end
  function words(s,fun1)
    fun1 = fun1 or function(x) return x end
    local t={};for x in s:gmatch(sep)do t[1+#t]=fun1(x)end;return t end
  lines(src, function(line) fun(words(line, coerce)) end) end

-- ## Start


help:gsub("\n [-][%S]+[%s]+[-][-]([%S]+)%s[^\n]+= ([%S]+)",
          function(k,x) the[k]=coerce(x)end)
chat(cli(the,help))
