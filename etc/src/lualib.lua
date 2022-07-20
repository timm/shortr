local l={}
-- Store old names (so, on last line, we can check for rogue locals)
local b4={}; for k,_ in pairs(_ENV) do l.b4[k]=k end
function l.rogues()
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end
  
l.big=math.huge 
l.min=math.min
l.max=math.max
l.fmt=string.format
l.rand=math.random

function l.any(a)  return a[l.rand(#a)] end
function l.per(t,p) p=p*#t//1; return t[math.max(1,math.min(#t,p))] end

function l.push(t,x)    t[1+#t]=x; return x end
function l.map(t,f,  u) u={};for _,x in pairs(t)do u[1+#u]=f(x)   end;return u end
function lkap(t,f,   u) u={};for k,x in pairs(t)do u[1+#u]=f(k,x) end;return u end
function l.sum(t,f,  u) u=0; for _,x in pairs(t)do u=u+f(x)       end;return u end

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

function cli(t,help)
  for k,v in pairs(t) do
    v = tostring(v)
    for n,x in ipairs(arg) do if x=="-"..(k:sub(1,1)) then 
      v = v=="false" and "true" or v=="true" and "false" or arg[n+1] end end
    t[k] =  l.coerce(v) end 
  if t.help then os.exit(help()) end
  return t end

function l.chat(t) print(l.cat(t)) return t end 
function l.cat(t,   show,u)  
  function show(k,v) return #t==0 and (":%s %s"):format(k,v) or tostring(v) end
  u={}; for k,v in pairs(t) do u[1+#u]=show(k,v) end
  return (t._is or "").."{"..table.concat(#t==0 and l.sort(u) or u," ").."}" end

function l.csv(file,fun)
  function lines(file, fun)
    local file = io.input(file)
    while true do
      local line = io.read()
      if not line then return io.close(file) else fun(line) end end 
  end ----------------------------
  function words(s,sep,fun,      t)
     fun = fun or same
     t={};for x in s:gmatch(fmt("([^%s]+)",sep)) do t[1+#t]=fun(x) end; return t 
  end -------------------------------------------------------------
  lines(file, function(line) fun(words(line, ",", l.coerce)) end) end 

----  obj(str,fun): class -- `Fun` is a constructor for instances of class `str`.
-- Polymorphism, encapsulation, classes, instance, constructors: all in 3 lines. :-)
function l.obj(txt,fun,  t,i) 
  local function new(k,...) i=setmetatable({},k); fun(i,...); return i end
  t={__tostring = l.cat}; t.__index = t;return setmetatable(t,{__call=new}) end

----  main(tab,tab) -- Runs some (or all) of the demos. Return number of failures.
-- Resets `the` and the random number seed before each demo. 
function l.main(the,go,help) 
  local the, fails, defaults = l.cli(the,help), 0, {}
  for k,v in pairs(the) do defaults[k]=v end 
  local todos = l.sort(l.kap(go,function(k,_) return k end))
  for _,todo in pairs(the.go=="all" and todos or {the.go}) do
    if type(go[todo])=="function" then
      for k,v in pairs(defaults) do the[k]=v end 
      math.randomseed(the.seed)
      if true ~= go[todo]() then 
        print("FAIL:",todo)
        fails=fails+1 end end end 
  l.rogues()
  os.exit(fails) end

return l
