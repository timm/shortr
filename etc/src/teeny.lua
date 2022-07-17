local the,help = {},[[
TODO: dont fuse on non-numerics

 -c cohen difference in nums    = .35
 -f file  source                = ../../data/auto93.csv
 -g go    action                = help
 -m min   size of small         = .5
 -s seed  random number seed    = 10019]]

big=math.huge 
min=math.min
max=math.max
fmt=string.format
rand=math.random

function sort(t,f) table.sort(t,f); return t end

function shuffle(t,   j)
  for i=#t,2,-1 do j=rand(i); t[i],t[j]=t[j],t[i] end; return t end

function coerce(x)
  x = x:match"^%s*(.-)%s*$" 
  if x=="true" then return true elseif x=="false" then return false 
  else return math.tointeger(x) or tonumber(x) or x end  end

help:gsub("\n ([-]%S)[%s]+([%S]+)[^\n]+= ([%S]+)",function(flag,key,x) 
  for n,arg1 in ipairs(arg) do 
    if   arg1==flag
    then x = x=="false" and "true" or x=="true" and "false" or arg[n+1] end end
  the[key] = coerce(x) end) 

function chat(t) print(cat(t)) return t end 
function cat(t)  
  local function show(k,v) return #t==0 and (":%s %s"):format(k,v) or tostring(v)  end
  local u={}; for k,v in pairs(t) do u[1+#u]=show(k,v) end
  return (t._is or "").."{"..table.concat(#t==0 and sort(u) or u," ").."}" end

function obj(txt,fun,  t) 
  local function new(k,...) local i=setmetatable({},k); fun(i,...); return i end
  t={__tostring = cat}; t.__index = t;return setmetatable(t,{__call=new}) end

local all={}
function all.table(t,fun) for k,v in pairs(t) do fun(t) end end

function all.csv(file,fun)
  function lines(file, fun)
    local file = io.input(file)
    while true do
      local line = io.read()
      if not line then return io.close(file) else fun(line) end end 
  end -----------------------------
  function words(s,sep,fun,      t)
     fun = fun or same
     t={};for x in s:gmatch(fmt("([^%s]+)",sep)) do t[1+#t]=fun(x) end; return t 
  end -------------------------------------------------------------
  lines(file, function(line) fun(words(line, ",", coerce)) end) end 

local NUM=obj("NUM",function(self,at,txt) 
  self.n=0; self.at=at or 0; self.txt=txt or ""; self.hi=-big; self.lo=big end)
function NUM:add(x) 
  if x~="?" then self.lo=min(self.lo,x); self.hi=max(self.hi,x) end end 

local SYM=obj("SYM",function(self,at,txt) 
  self.n=0; self.at=at or 0; self.txt=txt or ""; self.kept={} end)
function SYM:add(x) 
  if x~="?" then self.kept[x] = 1 + (self.kept[x] + 0) end end

local COLS=obj("COLS",function(self,  col)
  self.names=names; self.x, self.y, self.all= {},{},{}
  for k,v in pairs(names) do
    col= (v:find"^[A-Z]" and NUM or SYM)(at,txt)
    self.all[k]=col
    if v:find"[!+-]$" then self.y[k] = col else i.x[k] = col end end end)

function data(all,src,fun) -- all.csv,file or all.table,rows
  local i={rows={}, names={},nums = nil}
  all(src, function(row)
    if not i.nums then
      for k,v in pairs(row) do 
    else
      for k,n in pairs(i.nums) do 
      i.rows[ 1+#i.rows ] = row 
      if (#rows> the.wait) then 
        shuffle(i.rows)
        fun(i)
        i.rows={} end end end) end 
