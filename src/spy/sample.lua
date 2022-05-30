local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local help=[[  
SAMPLE: while not end of time, look around, see what's what
(c) 2022 Tim Menzies, timm@ieee.org, BSD2 license    
       
INSTALL: requires: lua 5.4+   
         download: sample.lua   
         test    : lua sample.lua -h   
         
USAGE: lua sample.lua [OPTIONS]   
                                              defaults   
                                              ~~~~~~~~   
  -S  --Seed  random number seed              = 10019   
  -G  --Goal  optimize for (helps,hurts,tabu) = helps   
  -b  --bins  number of bins                  = 16   
  -m  --min   min1 size (for pass1)           = .5   
  -M  --Min   min2 size (for pass2)           = 10
  -p  --p     distance coefficient            = 2   
  -s  --some  sample size                     = 512   
          
OPTIONS (other):   
  -f  --file  csv file with data = ../../etc/data/auto93.csv   
  -g  --go    start up action    = nothing   
  -h  --help  show help          = false
]]   
--------------------------------------------------------------------------------
-- Step1: make THE settings by combining help text with any command like flags.
function read(str)
  str = str:match"^%s*(.-)%s*$"
  if str=="true" then return true elseif str=="false" then return false end
  return math.tointeger(str) or tonumber(str) or str  end

local THE={}
help:gsub(" [-][-]([^%s]+)[^\n]*%s([^%s]+)",function(k,x) 
  for n,flag in ipairs(arg) do 
    if flag==("-"..key:sub(1,1)) or flag==("--"..key) then 
      x = x=="false" and"true" or x=="true" and"false" or arg[n+1] end end
  THE[k] = read(x) end ) 
  
math.random(THE.Seed)
--------------------------------------------------------------------------------
function str(i,       j)
  if type(i)~="table" or #i> 0 then return tostring(i) end
  if  #i>0 then return tostring(i) end
  j={}; for k,v in pairs(i) do j[1+#j] = string.format(":%s %s",k,v) end
  table.sort(j)
  return (i.is or "").."{"..table.concat(j," ").."}" end 

o   = function(i) print(str(i)) end
fmt = table.format
rand= math.random
seed= math.randomseed

--------------------------------------------------------------------------------
--[[
There are SYMs and NUMs and combinations there of.
--]]
local _id=0; id=function() _id=_id+1;return _id end

function klass(name,    t,new)
  function new(kl,...) local x=setmetatable({id=id()},kl); kl.new(x,...); return x end 
  t = {__tostring=str, is=name}; t.__index=t 
  return setmetatable(t, {__call=new}) end 

SYM=klass"SYM"
SOME=klass"SOME"
NUM=klass"NUM"

function col(i,holds,at,txt) 
  i.holds=holds
  i.n, i.at, i.txt = 0, i.at or 0, i.txt or ""
  i.w= i.txt:find"-$" and -1 or 1 end

function SOME.new(i, ...) col(i,{},...);     i.ok=false; end
function NUM.new( i, ...) col(i,SOME(),...); i.mu,i.lo,i.hi=0,big,-big end
function SYM.new( i, ...) col(i,{},...);     i.most, i.mode=0,nil end

function add(i,x, inc,fun)
  if x ~= "?" then
    inc = inc or 1
    i.n=i.n + inc;
    fun() end
  return x end

function SOME.add(x)
  return add(i,x, 1,function(     d)
    a = i.holds
    if     #a     < the.some     then i.ok=false; push(a,x)  
    elseif rand() < the.some/i.n then i.ok=false; a[rand(#a)]=x end end) end 

function NUM.add(x)
  return add(i,x, 1,function(     d)
    i.holds:add(x)
    f = x - i.mu
    i.mu = i.mu + d/i.mu
    i.hi=math.max(x, i.hi); i.lo=math.min(x, i.lo) end ) end

function SYM.add(x,inc)
  return add(i,x,inc, function()
    i.holds[x] = (inc or 1) + (i.holds[x] or 0)
    if i.holds[x] > i.most then i.most,i.mode = i.holds[x],x end end) end 

function SYM.merged(i,j,min,   k)
  k= SYM(i.at, i.txt)
  for x,n in pairs(i.holds) do k:add(x,n) end
  for x,n in pairs(j.holds) do k:add(x,n) end
  if i.n < min or j.n < min then return k end
  if k:div()*1.01 <= (i:div()*i.n + j:div()*j.n)/k.n  then return k end end