-- # OO
-- Misc utils

-- TABLE.OF.CONTENTS

-- ## Config
-- `b4` is a list of everything known before this code.  Used by `rogue()` (see below).
local b4={}; for k,v in pairs(_ENV) do b4[k]=k end
-- > help :str > Help text for this code. <
local help = [[

oo.lua : stuff that is cool
(c) 2022 Tim Menzies BSD-two-clause

 -c  cohen  difference in nums   = .35
 -f  file   source               = ../../data/auto93.csv
 -g  go     action               = help
 -h  help   show help            = false
 -m  min    size of small        = .5
 -s  seed   random number seed   = 10019
 -S  Some   some items to keep   = 256]]

-- > the :table > Config settings. Extracted from `help`. <
local the={}
help:gsub("\n [-]%S[%s]+([%S]+)[^\n]+= ([%S]+)", function(k,x) 
          if x=="true" then the[k]=true elseif x=="false" then the[k]=false 
          else the[k] = math.tointeger(x) or tonumber(x) or x end end )

-- ## Names
-- By defining names before the code, the code can be written in any order.
local cat,chat,cli,csv,fmt,kap,lines,map
local new,obj,per,push,R,rogues,same,sort,trim,words

-- > obj(txt:str,base:?class) :class > Make a class, perhaps as a kid of `base`.
-- Instances have a unique `id` and use the `cat` function for pretty printing.
local _id=0
function obj(txt,base,  t,new,i)
  function new(k,...) 
    _id=_id+1; i=setmetatable({_id=id},k); k.new(i,...); return i end
  t={__tostring=cat}
  for k,v in pairs(base or {}) do t[k] = v end
  t.is, t.__index =  txt, t
	return setmetatable(t,{__call=new}) end

-- In this code, ROWS hold many ROWs which are summarized in COLs (which can be either
-- SYMboliuc or NUMeric). SOME is a helper class for NUM that keeps a sample of the data.
local COL,ROW,ROWS   = obj"COL", obj"ROW", obj"ROWS"
local NUM, SOME, SYM = obj("NUM",COL), obj("SOME",COL), obj("SYM",COL) 

-- ## Columns
-- ### COL
-- #### Creation
-- > COL(at:?int=0, txt:?str=""): COL > Superclass constructor for columns. <
function COL:new(at,txt)
  self.at  = at or 0     
  self.txt = txt or ""  
  self.n   = 0 end     

-- #### Reports
-- > dist(x:any, y:any) :num > Return distance. For missing values, assume max distance. <
function COL:dist(x,y)
  return x=="?" and y=="?" and 1 or self:dist1(x,y) end

-- #### Update
-- > add(x:any, inc:?int=1) > `inc` times repeat: add `x` <
function COL:add(x,inc)
  if x ~= "?" then 
    inc = inc or 1
    self.n = self.n + inc
    self:add1(x,inc) end end

-- ### SOME
-- #### Creation
function SOME:new(...)
  COL.new(self, ...)
  self.kept,self.ok,self.max,self.n = {},true,the.Some,0  end

-- #### Update
function SOME:add1(x,inc)
  for j=1,inc do
    local a= self.kept
    if     #a  < self.max        then self.ok=false; push(a,x) 
    elseif R() < self.max/self.n then self.ok=false; a[R(#a)]=x end end end 

-- #### Reports
function SOME:has()
  self.kept = self.ok and self.kept or sort(self.kept)
  self.ok=true
  return self.kept  end

-- ### NUM
-- #### Creation
local NUM=obj("NUM",COL)
function NUM:new(...)
  COL.new(self, ...)
  self.kept = SOME()          
  self.w = self.txt:find"-$" and -1 or 1 end

-- #### Report
function NUM.div(i) 
  local a=i.kept:has(); return (per(a,.9) - per(a,.1))/2.56 end

function NUM.mid(i) 
  local a=i.kept:has(); return per(a,.5) end

function NUM:norm(x)
  local a = self.kept:has()
  return (a[#a]-a[1])<1E-9 or (x-a[1])/(a[#a]-a[1]) end

-- #### Update
function NUM:add1(x,inc)
  for j=1,inc do self.kept:add(x) end end 

-- ## Lib
-- ### Lint
-- > rogues() > Warn if our code introduced a rogue global. <
local function rogues()
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end end
-- ### Maths
R=math.random
-- ### Lists
function same(x)      return x end
function map(t,f,  u) u={};for _,x in pairs(t)do u[1+#u]=f(x) end;return u end
function kap(t,f,  u) u={};for k,x in pairs(t)do u[1+#u]=f(k,x)end;return u end
function sort(t,f)    table.sort(t,f); return t end
function push(t,x)    t[1+#t]=x; return x end
function per(t,p)     p=p*#t//1; return t[math.max(1,math.min(#t,p))] end


-- ### Misc
function cli(t)
  for key,x in pairs(t) do 
    x = tostring(x)
    for n,flag in ipairs(arg) do 
      if   flag=="-"..key:sub(1,1) 
      then x = x=="false" and "true" or x=="true" and "false" or arg[n+1] end end
    t[key] = thing(x) end 
  return t end

function same(x) return x end
-- ### String2things

function csv(file,fun)
  lines(file, function(line) fun(words(line, ",", thing)) end) end 

function lines(file, fun)
  local file = io.input(file)
  while true do
    local line = io.read()
    if not line then return io.close(file) else fun(line) end end end

-- > the :table > Config settings. Extracted from `help`. <
function thing(x) 
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x end 


function words(s,sep,fun,      t)
  fun = fun or same
  t={};for x in s:gmatch(fmt("([^%s]+)",sep)) do t[1+#t]=fun(x) end; return t end

-- ### Thing2string
function chat(t) print(cat(t)); return t end
function cat(t,   u,pub) 
  pub=function(k,v) return tostring(k):sub(1,1)~="_" end
  if type(t)~="table" then return tostring(t) end
  if #t>0             then return "{"..table.concat(map(t,tostring)," ").."}" end
  u={};for k,v in pairs(t) do if pub(k) then u[1+#u]=(":%s %s"):format(k,v) end end
  table.sort(u)
  return (t.is or "").."{"..table.concat(u," ").."}"  end

fmt=string.format

-- ### Testing
local go,fails={},0

function go.all() 
  local defaults={}
  for k,v in pairs(the) do defaults[k]=v end 
  local want = function(k,_)if k~="all" then return k end end
  for k,x in pairs(sort(kap(go,want))) do 
    for k,v in pairs(defaults) do the[k]=v end 
    math.randomseed(the.seed)
    if true ~= go[x]() then 
      print("FAIL:",k)
      fails=fails+1 end end end

function go.the()  chat(the);    return true end
function go.some( n) 
  n = NUM()
  chat(n)
  for j=1,10^3 do n:add(j) end
  chat(n.kept:has()) return true end

-- ## Start
the=cli(the)
if the.help then print(help) elseif go[the.go] then go[the.go]() end
rogues()
os.exit(fails)
