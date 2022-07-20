local l={}
-- Store old names (so, on last line, we can check for rogue locals)
local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end
function l.rogues()
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end end
  
l.big=math.huge 
l.min=math.min
l.max=math.max
l.fmt=string.format
l.rand=math.random

function l.any(a)       return a[l.rand(#a)] end
function l.many(a,n, u) u={}; for j=1,n do u[1+#u]= l.any(a) end;return u end

function l.per(t,p) p=math.floor((p*#t)+.5); return t[l.max(1,l.min(#t,p))] end

function l.push(t,x)    t[1+#t]=x; return x end
function l.map(t,f,  u) u={};for _,x in pairs(t)do u[1+#u]=f(x)   end;return u end
function l.kap(t,f,  u) u={};for k,x in pairs(t)do u[1+#u]=f(k,x) end;return u end
function l.sum(t,f,  u) u=0; for _,x in pairs(t)do u=u+f(x)       end;return u end

function l.rev(t)
  for i=1, math.floor(#t / 2) do t[i],t[#t-i+1] = t[#t-i+1],t[i] end
  return t end

---- rnd(num, places:int):num  -- Return `x` rounded to some number of `place`.
function l.rnd(x, places)  --   &#9312;
  local mult = 10^(places or 2)
  return math.floor(x * mult + 0.5) / mult end
---- rnds(t:num, places:?int=2):num -- Return items in `t` rounds to `places`. 
function l.rnds(t, places)
  local u={};for k,x in pairs(t) do u[k]=l.rnd(x,places or 2)end;return u end

function l.sort(t,f) table.sort(t,f); return t end
function l.lt(x)     return function(a,b) return a[x] < b[x] end end

function l.shuffle(t,   j)
  for i=#t,2,-1 do j=rand(i); t[i],t[j]=t[j],t[i] end; return t end

function l.coerce(x)
  x = x:match"^%s*(.-)%s*$" 
  if x=="true" then return true elseif x=="false" then return false 
  else return math.tointeger(x) or tonumber(x) or x end  end

function l.cli(t,help)
  for k,v in pairs(t) do
    v = tostring(v)
    for n,x in ipairs(arg) do if x=="-"..(k:sub(1,1)) then 
      v = v=="false" and "true" or v=="true" and "false" or arg[n+1] end end
    t[k] =  l.coerce(v) end 
  if t.help then 
    os.exit(print(help:gsub("[%u][%u%d]+","\27[1;36m%1\27[0m")
                      :gsub(" ([-]%S)",  " \27[1;31m%1\27[0m"))) end
  return t end

function l.chat(t) print(l.cat(t)) return t end 
function l.cat(t,   show,u)  
  if type(t)~="table" then return tostring(t) end
  function show(k,v) return #t==0 and (":%s %s"):format(k,v) or tostring(v) end
  u={}; for k,v in pairs(t) do u[1+#u]=show(k,v) end
  return (t._is or "").."{"..table.concat(#t==0 and l.sort(u) or u," ").."}" end

function l.csv(file,fun)
  local function lines(file, fun)
    local file = io.input(file)
    while true do
      local line = io.read()
      if not line then return io.close(file) else fun(line) end end end 
  local function words(s,sep,fun,      t)
    fun = fun or same
    t={};for x in s:gmatch(l.fmt("([^%s]+)",sep))do t[1+#t]=fun(x)end;return t end 
  lines(file, function(line) fun(words(line, ",", l.coerce)) end) end 

--- --- obj(str,fun): class -- `Fun` is a constructor for instances of class `str`.
-- Polymorphism, encapsulation, classes, instance, constructors: all in 3 lines. :-)
function l.obj(txt,fun,  t,i) 
  local function new(k,...) i=setmetatable({},k); fun(i,...); return i end
  t={__tostring = function(x) return txt..l.cat(x) end}
  t.__index = t;return setmetatable(t,{__call=new}) end

return l
