local b4,the,help = {},{},[[

TODO: dont fuse on non-numerics

 -c cohen difference in nums    = .35
 -f file  source                = ../../data/auto93.csv
 -g go    action                = help
 -m min   size of small         = .5
 -s seed  random number seed    = 10019]]

for k,v in pairs(_ENV) do b4[k]=k end
local cat,chat,fmt,kap,map
local new,obj,push,same,showSlots,sort,trim

function obj(txt,base)
  t={__tostring=showSlots}
  for k,v in pairs(base or {}) do t[k] = v end
  t.is, t.__index =  txt, t
	return setmetatable(t,{__call=new}) end

-- ## Lib
-- ### Lists
function same(x) return x end
function map(t,f,  u) u={};for _,x in pairs(t)do u[1+#u]=f(x) end;return u end
function kap(t,f,  u) u={};for k,x in pairs(t)do u[1+#u]=f(k,x)end;return u end
function sort(t,f)    table.sort(t,f); return t end
function push(t,x)    t[1+#t]=x; return x end

-- ### Misc
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

function same(x) return x end
-- ### Objects
local _id=0
function new(k,...) 
  _id=_id+1;local i=setmetatable({_id=id},k); k.new(i,...); return i end

function showSlots(t,    u,pub)
  pub = function(k,v) return tostring(k):sub(1,1)~="_" end
  u={}; for k,v in pairs(t) do if pub(k) then u[1+#u]=(":%s %s"):format(k,v) end end
  table.sort(u)
  return (t.is or "").."{"..table.concat(u," ").."}"  end

-- ### String2things

function csv(file,fun)
  lines(file, function(line) fun(words(line, ",", thing)) end) end 

function lines(file, fun)
  local file = io.input(file)
  while true do
    local line = io.read()
    if not line then return io.close(file) else fun(line) end end end

function thing(x)
  x=trim(x)
  if x=="true" then return true elseif x=="false" then return false 
  else return math.tointeger(x) or tonumber(x) or x end  end

function trim(x) return  x:match"^%s*(.-)%s*$" end

function words(s,sep,fun,      t)
  fun = fun or same
  t={};for x in s:gmatch(fmt("([^%s]+)",sep)) do t[1+#t]=fun(x) end; return t end

-- ### Thing2string
function chat(t) print(cat(t)); return t end
function cat(t, u) 
  u={};for k,v in pairs(t) do u[1+#u]=tostring(k) end
  return "{"..table.concat(u," ").."}" end

fmt=string.format

-- ### Testing
local go={}
function go.all() 
  local want = function(k,_)if k~="all" then return k end end
  for _,x in pairs(sort(kap(go,want))) do 
    math.randomseed(the.seed)
    go[x]() end end

-- ## Demos

-- ## Start
help:gsub("\n [-]%S[%s]+([%S]+)[^\n]+= ([%S]+)",function(k,x) the[k]=thing(x) end)

the=cli(the)
go[the.go]()
rogues()
os.exit(fails)
