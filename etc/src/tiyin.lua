local b4={}; for k,v in pairs(_ENV) do b4[k]=k end
local cat,chat,cli,csv,fmt,kap,lines,map,new
local obj,order,push,rogues,same,sort,thing,trim,words
local the,help = {},[[

 -b bins  max number of bins    = 7
 -c cohen difference in nums    = .35
 -f file  source                = ../../data/auto93.csv
 -g go    action                = help
 -m min   size of small         = .5
 -s seed  random number seed    = 10019
]]
function trim(x) return  x:match"^%s*(.-)%s*$" end

function thing(x)
  if x=="true" then return true elseif x=="false" then return false 
  else return math.tointeger(x) or tonumber(x) or x end  end

help:gsub("\n [-]%S[%s]+([%S]+)[^\n]+= ([%S]+)", 
          function(k,x) the[k]=thing(trim(x)) end)

function lines(file, fun)
  local file = io.input(file)
  while true do
    local line = io.read()
    if not line then return io.close(file) else fun(line) end end end

function words(s,sep,fun,      t)
   fun = fun or same
   t={};for x in s:gmatch(fmt("([^%s]+)",sep)) do t[1+#t]=fun(x) end; return t end

function csv(file,fun)
  lines(file, function(line) fun(words(line, ",", thing)) end) end 

function cli(t)
  for key,x in pairs(t) do 
    x = tostring(x)
    for n,flag in ipairs(arg) do 
      if   flag=="-"..key:sub(1,1) 
      then x = x=="false" and "true" or x=="true" and "false" or arg[n+1] end end
    t[key] = thing(x) end 
  return t end

function rogues()
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end end

fmt=string.format
function same(x) return x end
function map(t,f,     u) u={};for _,x in pairs(t) do u[1+#u]=f(x) end;return u end
function kap(t,f,     u) u={};for k,x in pairs(t) do u[1+#u]=f(k,x) end;return u end
function sort(t,f)       table.sort(t,f); return t end
function push(t,x)       t[1+#t]=x; return x end

function chat(t) print(cat(t)); return t end
function cat(t)
  local function pub(k,v) return (tostring(k)):sub(1,1)~="_" end
  local function key(k,v) if pub(k) then  return fmt(":%s %s",k,v) end end
  local u=  #t>1 and map(t,f or tostring) or sort(kap(t,key))
  return (t._is or "").."{"..table.concat(u," ").."}" end

local _id = 0
function new(kl,...) 
  local x
  _id=_id+1; x=setmetatable({_id=_id},kl);kl.new(x,...); return x end 

function obj(name)
  local t = {__tostring=cat,_is=name}; t.__index=t
  return setmetatable(t, {__call=new}) end

------------------------------------------------------
local function col(self, at,txt)
  self.at   = at or 0                 -- :num   column position 
  self.txt  = txt or ""               -- :str   column name 
  self.n    = 0  end                  -- :num   items seen so far
 
local SYM = obj"SYM"
function SYM:new(at,txt); self.kept={}; col(self,at,txt) end

function SYM:add(x) 
  if x ~= "?" then
    self.n=self.n+1
    self.kept[x] = 1+(self.kept[x] or 0) end end

function SYM:bins(rows) return true end
function SYM:ent(    e)
  local function z(p) return  p*math.log(p,2) end
  e=0;for _,n in pairs(self.kept) do if n>0 then e=e-z(n/i.n) end end;return e end

local NUM = obj"NUM"
function NUM:new(at,txt) 
  col(self,at,txt)
  self.lo =math.huge; self.hi=-self.lo 
  self.mu, self.m2, self.sd = 0,0,0
  self.w = self.txt:find"-$" and -1 or 1  end

function NUM:add(x)
  if x ~= "?" then
    self.n  = self.n+1
    local d = x - self.mu
    self.mu = self.mu + d/self.n
    self.m2 = self.m2 + d*(x - self.mu)
    self.sd = (self.n < 2 or self.m2<0) and 0 or ((self.m2)/(self.n -1))^.5 
    if x > self.hi then self.hi = x end
    if x < self.lo then self.lo = x end end end

function NUM:bins(rows)
  local function lt(x,y) 
    return (x=="?" and -math.huge or x) < (y=="?" and -math.huge or y) end
  local order   = function(a,b) return lt(a.raw[self.at],b.raw[self.at]) end
  local n,bin,b4= 1,1,nil
  local x       = function(k)   return rows[k].raw[self.at] end
  local xis     = function(k,x) rows[k].cooked[self.at] = x end
  for j,row in pairs(sort(rows, order)) do
    if x(j) ~= "?" then 
      b4 = b4 or j
      if   x(j) - x(b4) > self.sd*the.cohen and n > self.n^the.min 
      then bin=bin+1; n=0; b4=j end 
      n = n+1
      xis(j,bin) end end end

local is={}
is.skip=  function(x) return x:find":$"     end -- what to ignore
is.klass= function(x) return x:find"!$"     end -- single goal
is.goal=  function(x) return x:find"[!+-]$" end -- dependent column
is.num=   function(x) return x:find"^[A-Z]" end -- NUM or SYM?

local COLS = obj"COLS"
function COLS:new(names) 
  self.names = names   -- :[str]       list of known columns names
  self.all   = {}      -- :[NUM|SYM]   all the columns
  self.x     = {}      -- :[NUM|SYM]   list of pointers to just the independent columns
  self.y     = {}      -- :[NUM|SYM]   list of ponters to just the dependent columns
  self.klass = nil     -- :?(NUM|SYM)  pointer to the klass column, may be nil.
  for at,txt in pairs(names) do 
    local col = (is.num(txt) and NUM or SYM)(at,txt) 
    push(self.all, col)
    if not is.skip(txt) then
      push(is.goal(txt) and self.y or self.x, col)
      if is.klass(txt) then self.klass = col end end end end

function COLS:add(row)
  for _,cols in pairs{self.x,self.y} do
    for _,col in pairs(cols) do col:add(row.raw[col.at]) end end 
  return row end

local ROW = obj"ROW"
function ROW:new(of,cells) 
  self.raw   = cells 
  self.cooked= cells   
  self._of    = of 
  self.evaled = false end

local ROWS = obj"ROWS"
function ROWS:new() self.rows={}; self.cols=nil end

function ROWS:clone(src) return ROWS():add(self.cols.names):adds(src) end

function ROWS:adds(src)
  if   type(src) == "string" 
  then csv(src, function(row) self:add(row) end)
  else for _,row in pairs(src or {}) do self:add(row) end end
  return self end 

function ROWS:add(row)
  if   self.cols 
  then push(self.rows,  self.cols:add( row.raw and row or ROW(self,row))) 
  else self.cols = COLS(row) end end

local go={}
function go.all() 
  local want = function(k,_)if k~="all" then return k end end
  for _,x in pairs(sort(kap(go,want))) do 
    math.randomseed(the.seed)
    go[x]() end end

function go.help() print(help) end
function go.the()  chat(the) end
function go.csv()  csv(the.file, function(x) chat(x) end) end
function go.rows(r)  
  print(the.file)
  r=ROWS():adds(the.file) 
  chat(r.cols.x[4])
  for _,col in pairs(r.cols.x) do col:bins(r.rows) end
  for _,row in pairs(r.rows) do chat(row.cooked) end
  end

the=cli(the)
go[the.go]()
rogues()
