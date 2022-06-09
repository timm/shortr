local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local lib={}

lib.big  = math.huge
lib.fmt  = string.format
lib.fmtp = function(...) print(fmt(...)) end 
lib.rand = math.random

function lib.argmax(t,f,  max,n,tmp)
  arg, max = nil, -lib.big
  for key,x in pairs(t) do tmp=f(x); if tmp>max then arg,max = key,tmp end end
  return arg end

function lib.cli(t,help)
  for key,x in pairs(t) do
    x = lib.str(x)
    for n,flag in ipairs(arg) do 
      if flag==("-"..key:sub(1,1)) or flag==("--"..key) then 
         x= x=="false" and"true" or x=="true" and"false" or arg[n+1] end end 
    t[key] = lib.read(x) end 
  if t.help then os.exit(print(help:gsub("[%u][%u%d]+","\27[1;31m%1\27[0m"))) end
  return t end

function lib.csv(csvfile) 
  csvfile = io.input(csvfile)
  return function(s, t) 
    s=io.read()
    if not s then io.close(csvfile) else
      t={}; for x in s:gmatch("([^,]+)") do t[1+#t] = lib.read(x) end
      return t end end end 

function lib.copy(t,   u)
  if type(t) ~= "table" then return t end
  u={};for k,v in pairs(t) do u[lib.copy(k)]=lib.copy(v) end
  return setmetatable(u, getmetatable(t)) end

function lib.demos(THE,go)
  local fails,backup = 0,{}
  for k,v in pairs(THE) do backup[k]=v end 
  for txt,todo in pairs(THE.go=="all" and go or {go[THE.go]}) do
    if type(todo)=="function"  then 
      for k,v in pairs(backup) do THE[k]=v end 
      math.randomseed(THE.seed)               
      io.write(".")
      local result = todo()
      if result ~= true then         
        fails = fails + 1
        print("--Error",txt,status) end end end
  for k,v in pairs(_ENV) do  if not b4[k] then print("?",k,type(v)) end end 
  os.exit(fails)  end

function lib.gt(x) return function(a,b) return a[x] > b[x] end end

function lib.is(name,    t,new,x)  
  function new(kl,...) x=setmetatable({},kl);kl.new(x,...); return x end 
  t = {__tostring=lib.str, is=name}; t.__index=t
  return setmetatable(t, {__call=new}) end 

function lib.map(t,f,  u) 
  u={}; for k,v in pairs(t) do u[1+#u]=(f and f(v) or v) end return u end

function lib.normpdf(x, mu, sd,      denom,nom)
 return sd==0 and (x==mu and 1 or 0) or
   math.exp(-1*(x - mu)^2/(2*sd^2)) * 1 / (sd * ((2*math.pi)^0.5)) end

function lib.oo(i) print(lib.str(i)) end

function lib.pop(t)  return table.remove(t) end

function lib.push(t,x) t[1+#t] = x ; return x end

function lib.read(str) 
  str = str:match"^%s*(.-)%s*$"
  if str=="true" then return true elseif str=="false" then return false end
  return math.tointeger(str) or tonumber(str) or str  end

function lib.rnd(n, p) local m=10^(p or 2); return math.floor(n*m+0.5)/m  end

function lib.shuffle(t,    j)
  for i = #t, 2, -1 do j=math.random(i); t[i], t[j] = t[j], t[i]; end;
  return t end

function lib.same(x) return x end

function lib.splice( t, i, j, k,    u) 
  u={};for n=(i or 1)//1,(j or #t)//1,(k or 1)//1 do u[1+#u]=t[n] end;return u end

function lib.sort(t,f) table.sort(t,f); return t end

function lib.str(i,    j) 
  if type(i)~="table" then return tostring(i) end
  if #i> 0 then j= lib.map(i,tostring) 
  else j={}; for k,v in pairs(i) do j[1+#j] = string.format(":%s %s",k,v) end
       table.sort(j) end
  return (i.is or "").."{"..table.concat(j," ").."}" end 

function lib.sum(t,f,   n) n=0; for _,v in pairs(t) do n=n+f(v) end; return n end

return lib
