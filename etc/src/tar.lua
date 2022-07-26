local the,help={},[[

TAR3: recursively find grow, combine, useful rangess

OPTIONS:
 -b  --bins  number of bins                           = 16
 -e  --era   process data in "era"s of size, say, 256 = 256
 -s  --seed  random number seed                       = 10019
 -S  --samples how many to check                      = 4
 -h  --help  show help                                = false

Boolean flags need no arguments e.g. "-h" sets "help" to "true".   ]]

---- ---- ---- ---- Names
local any,cat,chat,cli,coerce,csv,eras,fmt
local many,push,obj

_id =  0
function obj(txt,fun, i)
  local function new(k,...) 
    _id = _id + 1
    i=setmetatable({id=_id},k);
    fun(i,...); return i end
  local t={__tostring = function(x) return txt..cat(x) end}
  t.__index = t;return setmetatable(t,{__call=new}) end

---- ---- ---- Categories
---- ---- Columns
-- Summarize stream of symbols
local SYM=obj("SYM",function(self, at,txt) 
  self.n   = 0          -- number of items seen
  self.at  = at or 0    -- column number
  self.txt = txt or ""  -- column name
  self.symp= true
  self.kept= {}   end)   -- counters for symbols

---- NUM(?num=0, ?str="") -- Summarize streams of numbers in ROWs
local NUM=obj("NUM",function(self, at,txt)
  self.n   = 0                        -- number of items seen
  self.at  = at   or 0                -- column number
  txt=txt or ""
  self.txt = txt                      -- column name
  self.w   = txt:find"-$" and -1 or 1 -- If minimizing, then -1. Else 1
  self.kept= {}                       -- some sample of the seen items
  self.ok  = false end )              -- true if sorted, set to false by each add

---- ---- ROWS
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
  self.rows={dict={},list={}}
  self.cols=nil end)

local ROW=obj("ROW",function(self, of,raw)
  self._of=of
  self.raw=raw
  self.cooked=raw end)

local XY=obj("XY", function(self,col,xlo,xhi,y)
  self.xlo=xlo
  self.xhi=xhi or xlo
  self.y = y or SYM(col.at,col.txt)  end)

---- ---- ---- ---- methods
---- ---- ---- ROW
function ROW:__lt(other)
  self.evaled, other.evaled = true, true
  local s1, s2, ys, e = 0, 0, self._or.cols.y, math.exp(1)
  for _,col in pairs(ys) do
    local x = col:norm(self.raw[col.at])
    local y = col:norm(other.raw[col.at])
    s1      = s1 - e^(col.w * (x-y)/#ys)
    s2      = s2 - e^(col.w * (y-x)/#ys) end
  return s1/#ys < s2/#ys end -- i.e. we lose less going to r2->r1 than r2->r1

function ROW:__sub(other)
  local d,x1,x2 = 0
  for _,col in pairs(self._of.cols.x) do 
    x1 = r1.raw[col.at]
    x2 = r2.cells[col.at]
    d  = d+(x1 -x2)^the.p end
  return (d/#self.x)^(1/the.p) end

---- ---- ---- ROWS
-- add many items
function ROWS:adds(src)
  if   type(src)=="table" 
  then for _,row in pairs(src) do self.cols:add(row) end
  else csv(src, function(row)
         if self.cols 
         then row = self.cols:add(row)
              self.rows.dict[row.id] = push( self.rows.list, row)
         else self.cols = COLS(row) end end) end end

function ROWS:best()
  some  = sort(many(shuffle(self.rows.list),the.few)) 
  close = (some[1] - some[2])/2
  d13   = some[1] - some[#some - 1]
  d14   = some[1] - some[#some]
  far   = (d13+d14)/2
  rows={}
  for _,row in pairs(self.rows) do
    if row-some[1]     < close then push(rows,row).label=true end
    if row-some[#some] > far   then push(rows,row).label=false end end end

---- ---- ---- COLS
function COLS:add(row)
  row = row.raw and row or ROW(row)
  for _,cols in pairs{self.x,self.y} do
    for col in pairs(cols) do  col:add(row.raw[col.at]) end end 
  return row end

---- ---- ---- SYM
function SYM:add(x,n) 
  n = n or 1
  if x~="?" then self.n=self.n+n; self.kept[x]=n + (self.kept[x] or 0) end end

function SYM:dist(x,y) return (x=="?" or  y=="?") and 1 or x==y and 0 or 1 end

function SYM:div() 
  return sum(self.kept, function(n) return -n/self.n*math.log(n/self.n,2) end) end

---- ---- ---- NUM
function NUM:add(x)
  if x~="?" then 
    self.n = self.n + 1
    local pos
    if #self.kept < the.Some        then pos= (#self.kept)+1 
    elseif rand() < the.Some/self.n then pos= rand(#self.kept) end
    if pos then 
      self.ok=false  -- the `kept` list is no longer in sorted order
      self.kept[pos]=x end end end

function NUM:has()
  self.kept = self.ok and self.kept or sort(self.kept)
  self.ok = true
  return self.kept end

function NUM:div() return (per(self:has(),.9) - per(self:has(),.1))/2.58 end
function NUM:mid() return per(self:has(),.5) end

function NUM:norm(x)
  local a =  self:has()
  local lo,hi = a[1], a[#a]
  return x=="?" and x or math.abs(hi-lo)<1E-9 and 0 or (x-lo)/(hi-lo+1/big) end

----------------
-- XXX move bins into xplan. add symp to sym
function XY.merged(i,j, min)
  local y = SYM(i.at, i.txt)
  for x,n in pairs(i.kept) do k:add(x,n) end
  for x,n in pairs(j.kept) do k:add(x,n) end
  if i.n < min or j.n < min or k:div() <= (i.n*i:div() + j.n*j:div())/k.n 
  then return XY(i.col, i.xlo, j.xhi, y, rows) end end

function bins(rows,col)
  local function where(x,     a,b,lo,hi)
    a = col:has()
    lo,hi = a[1], a[#a]
    b = (hi - lo)/the.bins
    return hi==lo and 1 or math.floor(x/b+.5)*b 
  end -------------------
  local function merges(b4,min) 
    local n,now = 1,{}
    while n <= #b4 do
      local merged = n<#b4 and b4[n]:merged(b4[n+1],min) -- defined in BIN
      now[#now+1]  = merged or b4[n]
      n            = n + (merged and 2 or 1)  -- if merged, skip over merged bin
    end -- end while
    if #now < #b4 then return merges(now,min) end     -- seek others to merge
    now[1].lo, now[#now].hi = -big,big            -- grow to plus/minus infinity
    return now 
  end -------- 
  local n,dict,list = 0,{},{}
  for _,row in pairs(rows) do
    local v = row.raw[col.at]
    if v ~= "?" then
      n=n+1
      local bin = col.symp and v or where(col,v) or v
      dict[bin] = dict[bin] or push(list, XY(col,v))
      local it  = dict[bin]
      it.xlo = math.min(x,it.xlo)
      it.xhi = math.max(x,it.xhi)
      it.y:add(y) end end
  list = sort(list,lt"lo")
  return col.symp and list or merges(list, n^the.min) end

---- ---- Distance
---- NUM:dist(x,y):num -- Normalize x,y to 0..1, report their difference.
-- If any unknowns, assume max distance.
function NUM:dist(x,y)
  if x=="?" and y=="?" then return 1
  elseif x=="?" then y=self:norm(y); x=y<.5 and 1 or 0 
  elseif y=="?" then x=self:norm(x); y=x<.5 and 1 or 0
  else   x,y = self:norm(x), self:norm(y) end
  return math.abs(x-y) end



---- ---- ---- ---- Lib
---- ---- ---- Lists
function push(t,x) t[1+#t]=x; return x end

function per(t,p) 
  p=math.floor((p*#t)+.5); return t[math.max(1,math.min(#t,p))] end

function shuffle(t,   j)
  for i=#t,2,-1 do j=l.rand(i); t[i],t[j]=t[j],t[i] end; return t end

function any(a) return a[l.rand(#a)] end

function many(a,n, u) u={}; for j=1,n do u[1+#u]= any(a) end;return u end


---- ---- ---- Thing to string
fmt=string.format

function cat(t)
  if type(t)~="table" then return tostring(t) end
  local function pub(k) return "_"~=tostring(k):sub(1,1) end
  local function show(k,v) 
          if pub(k) then return #t==0 and fmt(":%s %s",k,v) or tostring(v) end end
  local u={}; for k,v in pairs(t) do u[1+#u]=show(k,v) end
  if #t==0 then table.sort(u) end
  return (t._is or "").."{"..table.concat(u," ").."}" end

function chat(t) print(cat(t)) return t end

---- ---- ---- String to thing
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

---- ---- ---- ---- Start
help:gsub("\n [-][%S]+[%s]+[-][-]([%S]+)%s[^\n]+= ([%S]+)",
          function(k,x) the[k]=coerce(x)end)
chat(cli(the,help))
