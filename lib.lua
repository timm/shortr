local b4={}; for x,_ in pairs(_ENV) do b4[x]=x end 
local lib={}

lib.big = math.huge
lib.fmt = string.format
lib.R   = math.random

local _id=0
function lib.id() _id = 1 + _id; return _id end

function lib.push(t,x)    t[1+#t]=x; return x end
function lib.sort(t,f)    table.sort(t,f); return t end
function lib.map(t,f,  u) u={}; for _,v in pairs(t) do u[1+#u]=f(v) end; return u end
function lib.sum(t,f,  n) n=0;  for _,v in pairs(t) do n = n + f(v) end; return n end
function lib.per(t,p)     p=p*#t//1; return t[math.max(1,math.min(#t,p))] end
function lib.lt(x)        return function(a,b) return a[x] < b[x] end end

function lib.argmax(t,f)
  local arg, max = nil, -lib.big
  for key,x in pairs(t) do local tmp=f(x);if tmp>max then arg,max=key,tmp end end
  return arg end

function lib.o(t) 
  if type(t) ~= "table" then return tostring(t) end
  if #t>0 then return "{"..table.concat(lib.map(t,lib.o)," ").."}" end
  local u,pub
  function pub(x) return "_"~=tostring(x):sub(1,1) end
  u={}; for x,v in pairs(t) do 
          if pub(x) then u[1+#u]=lib.fmt(":%s %s",x,lib.o(v)) end end
  return "{"..table.concat(lib.sort(u)," ").."}" end 

function lib.oo(t) print(lib.o(t)); return t end

function lib.cli(t,help)
  for key,x in pairs(t) do
    x = tostring(x)
    for n,flag in ipairs(arg) do 
      if flag==("-"..key:sub(1,1)) or flag==("--"..key) then 
         x= x=="false" and"true" or x=="true" and"false" or arg[n+1] end end 
    t[key] = lib.atom(x) end 
  if t.help then os.exit(print(help:gsub("[%u][%u%d]+","\27[1;32m%1\27[0m"),"")) end
  return t end

function lib.atom(str) 
  str = str:match"^%s*(.-)%s*$"
  if str=="true" then return true elseif str=="false" then return false end
  return math.tointeger(str) or tonumber(str) or str  end

function lib.csv(csvfile) 
  csvfile = io.input(csvfile)
  return function(s, t) 
    s=io.read()
    if not s then io.close(csvfile) else
      t={}; for x in s:gmatch("([^,]+)") do t[1+#t] = lib.atom(x) end
      return t end end end 

function lib.csv(file) 
  file = io.input(file)
  return function(s, t) 
    s=io.read()
    if not s then io.close(file) else
      t={};for x in s:gmatch("([^,]+)")do t[1+#t]=lib.atom(x)end;return t end end end 

function lib.splice( t, start, stop, step,    u) 
  u={}
  for n=(start or 1)//1,(stop or #t)//1,(step or 1)//1 do u[1+#u]=t[n] end
  return u end

function lib.demos(the,go)
  local fails,backup = 0,{}
  for x,v in pairs(the) do backup[x]=v end 
  for txt,todo in pairs(the.go=="all" and go or {go[the.go]}) do
    if type(todo)=="function"  then 
      for x,v in pairs(backup) do the[x]=v end 
      math.randomseed(the.seed)               
      io.write(".")
      local result = todo()
      if result ~= true then         
        fails = fails + 1
        print("--Error",txt,status) end end end
  for x,v in pairs(_ENV) do  if not b4[x] then print("?",x,type(v)) end end 
  os.exit(fails)  end

return lib
