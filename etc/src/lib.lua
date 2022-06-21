-- ## Misc library routines
local m={}
-- ### Linting

--> rogues() -> Find rogue locals. Run `rogues()` _last_ after everything else.
local b4={}; for k,v in pairs(_ENV) do b4[k]=k end
function m.rogues()
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end end
-- ###  Lists

--> sort(t:tab, f:fun) :tab -> Return `t`, sorted of function `f` (default "<").
function m.sort(t,f) table.sort(t,f); return t end
--> push(t:tab, x:any) :x -> Add `x` to end of `t`; return `t`.
function m.push(t,x) t[1+#t] = x; return x end
--> per(t:tab, p:?float=.5) :x -> Return `p`-th ranked item from `t`.
function m.per(t,p) p=p*#t//1; return t[math.max(1,math.min(#t,p))] end

--> same(x:any):any -> Return x, unchanged.
m.same=function(x) return x end

--> map(t:tab, f:fun): tab -> 
--> kap(t:tab, f:fun): tab -> 
--> maps(list1:tab, list2:tab, f:fun): tab -> 
--> kaps(list1:tab, list2:tab, f:fun): tab -> Return items in `t`, filtered thru `f`.
-- If `f` returns nil, then the output table shrinks. `kap` and `kaps` pass the
-- key and value to `f`. `maps` and `kaps` pass items from two lists.
function m.map(t,f,     u) u={};for _,x in pairs(t) do u[1+#u]=f(x) end;return u end
function m.kap(t,f,     u) u={};for k,x in pairs(t) do u[1+#u]=f(k,x) end;return u end
function m.maps(t,u,f,  v) v={};for k,x in pairs(t) do v[1+#v]=f(x,u[k]) end;return v end
function m.kaps(t,u,f,  v) v={};for k,x in pairs(t) do v[1+#v]=f(k,x,u[k]) end;return v end
-- ### String to thing

--> thing(s:str):any -> Coerce string to whatever
-- is simplest (boolean or integer or float or, if all else fails, a string).
function m.thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false else
    return math.tointeger(x) or tonumber(x) or x end  end

--> words(s:str, sep:str, fun:fun):tab -> Return `t` filled with `s`, split  on `sep`.
function m.words(s,sep,fun,      t)
   fun = fun or m.same
   t={};for x in s:gmatch(m.fmt("([^%s]+)",sep)) do t[1+#t]=fun(x) end; return t end

--> csv(file:str,  fun:fun):tab -> Call `fun` with lines, split on ",".
function m.csv(file, fun)
  local file = io.input(file)
  while true do
    local line = io.read()
    if not line then return io.close(file) else fun(m.words(line, ",",m.thing)) end end end
-- ### Thing to string

--> fmt(s:str,...) :str -> emulate prinft
m.fmt=string.format

--> cat(t:tab):str -> Return table as string. For key-indexed lists, show keys (sorted).
function m.cat(t,    key,u)
  function key(k,v) if (tostring(k)):sub(1,1)~="_" then return m.fmt(":%s %s",k,v) end end
  u=  #t>1 and m.map(t,f or tostring) or m.sort(m.kap(t,key))
  return (t._is or "").."{"..table.concat(u," ").."}" end

--> chat(t:tab):t -> Print table (as string). Return `t`.
function m.chat(t) print(m.cat(t)); return t end
-- ### Settings

--> opts(x:str) :tab -> Parse `str` for lines with `--`; then pull keys+defaults. 
function m.opts(x)
  local t = {}
  x:gsub("\n  ([-][^%s]+)[%s]+([-][-]([^%s]+))[^\n]*%s([^%s]+)",
           function(f1,f2,k,x) t[k] = m.thing(x) end)
  t._HELP = x
  return t end

--> cli(t:tab) :tab -> For keys in `t`, look for updates on command-line. 
-- Things with boolean defaults are flipped via `--flag`. 
-- Other keys need `--flag value`.  Print the help
-- (if `-h` appears on command line). Return a table with setting `key`s and
-- `value`s.
function m.cli(the)
  for key,x in pairs(the) do 
    x = tostring(x)
    for n,flag in ipairs(arg) do 
      if flag=="-"..key:sub(1,1) or flag=="--"..key  then
        x = x=="false" and"true" or x=="true" and"false" or arg[n+1] 
        t[k] = m.thing(x) end end end
    if t.help then print(t._HELP:gsub("[%u][%u%d]+","\27[1;32m%1\27[0m"),"") end
  return t end
-- ### Tests

--> goes(settings:tab, tests:[fun]) -> Run some tests.
-- Before each test, reset random seed settings. 
function m.goes(settings,tests)
  local fails, old = 0, {}
  for k,v in pairs(settings) do old[k]=v end
  local tmp = settings.go=="all" and tests or {[settings.go]=tests[settings.go]}
  for k,v in pairs(tmp) do
    for k,v in pairs(old) do settings[k]=v end end -- reset settings to default
    math.randomseed(settings.seed or 10019)        -- reset seed to default
    local out = v()
    if out ~= true then fails = fails+1; print(fmt("FAIL: [%s] %s",k,out or "")) end
  os.exit(fails) end -- if fails==0 then our return code to the OS will be zero.
-- ### Objects

--> obj(name:str, fun:fun):object -> Return a klass `name` with constructor `fun`.
-- Add a unique `id` and a `tosting` method (that uses `cat` (above).
local _id = 0
function m.obj(name,fun,    t,new,x)
  function new(kl,...) _id=_id+1; x=setmetatable({_id=_id},kl);fun(x,...); return x end 
  t = {__tostring=m.cat,_is=name}; t.__index=t
  return setmetatable(t, {__call=new}) end
-- ### Return

return m
