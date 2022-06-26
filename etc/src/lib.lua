-- ## Library Functions
local lib={}
-- ### Linting

--> rogues() -> Find rogue locals. Run `rogues()` _last_ after everything else.
local b4={}; for k,v in pairs(_ENV) do b4[k]=k end
function lib.rogues()
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end end
-- ### Meta

--> lt(x:str):function -> 
--> gt(x:str):function -> Returns functions that sorts on `x`
function lib.lt(x) return function(a,b) return a[x] < b[x] end end
function lib.gt(x) return function(a,b) return a[x] > b[x] end end

--> same(x:any):any -> Return x, unchanged.
lib.same=function(x) return x end

-- ### Maths

--> big:num -> return a big number
lib.big = math.huge

--> R(max:?num=1):num -> return a random number `0..max`.
lib.R = math.random

--> rnd(x:num, places:int):num -> return `x` rounded to some number of `places`.
function lib.rnd(x, places)
  local mult = 10^(places or 2)
  return math.floor(x * mult + 0.5) / mult end

--> rnds(t:num, places:?int=2):t -> return items in `t` rounds to `places`.
function lib.rnds(t, places)
  local u={};for k,x in pairs(t) do u[k]=lib.rnd(x,places or 2)end;return u end
-- ###  Lists

--> splice(t:tab,start=?int=1,stop:?num=#t,step:?num=1):t -> pull items
-- `start` to `stop`, stepping by `step`. 
function lib.splice(t, start, stop, step)
  local u={}
  for n=(start or 1)//1,(stop or #t)//1,(step or 1)//1 do u[1+#u]=t[n] end
  return u end

--> sort(t:tab, f:fun) :tab -> Return `t`, sorted of function `f` (default "<").
function lib.sort(t,f) table.sort(t,f); return t end
--> push(t:tab, x:any) :x -> Add `x` to end of `t`; return `t`.
function lib.push(t,x) t[1+#t] = x; return x end
--> per(t:tab, p:?float=.5) :x -> Return `p`-th ranked item from `t`.
function lib.per(t,p) p=p*#t//1; return t[math.max(1,math.min(#t,p))] end
--> map(t:tab, f:fun): tab -> 
--> kap(t:tab, f:fun): tab -> 
--> maps(list1:tab, list2:tab, f:fun): tab -> 
--> kaps(list1:tab, list2:tab, f:fun): tab -> Return items in `t`, filtered thru `f`.
-- If `f` returns nil, then the output table shrinks. `kap` and `kaps` pass the
-- key and value to `f`. `maps` and `kaps` pass items from two lists.
function lib.map(t,f,     u) u={};for _,x in pairs(t) do u[1+#u]=f(x) end;return u end
function lib.kap(t,f,     u) u={};for k,x in pairs(t) do u[1+#u]=f(k,x) end;return u end
function lib.maps(t,u,f,  v) v={};for k,x in pairs(t) do v[1+#v]=f(x,u[k]) end;return v end
function lib.kaps(t,u,f,  v) v={};for k,x in pairs(t) do v[1+#v]=f(k,x,u[k]) end;return v end
-- ### String to thing

--> thing(s:str):any -> Coerce string to whatever
-- is simplest (boolean or integer or float or, if all else fails, a string).
function lib.thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false else
    return math.tointeger(x) or tonumber(x) or x end  end

--> words(s:str, sep:str, fun:fun):tab -> Return `t` filled with `s`, split  on `sep`.
function lib.words(s,sep,fun,      t)
   fun = fun or lib.same
   t={};for x in s:gmatch(lib.fmt("([^%s]+)",sep)) do t[1+#t]=fun(x) end; return t end

--> csv(file:str,  fun:fun):tab -> Call `fun` with lines, split on ",".
function lib.csv(file, fun)
  local file = io.input(file)
  while true do
    local line = io.read()
    if not line then return io.close(file) else 
      fun(lib.words(line, ",", lib.thing)) end end end
-- ### Thing to string

--> fmt(s:str,...) :str -> emulate prinft
lib.fmt=string.format

--> cat(t:tab):str -> Return table as string. For key-indexed lists, show keys (sorted).
function lib.cat(t,    key,u)
  function key(k,v) if (tostring(k)):sub(1,1)~="_" then return lib.fmt(":%s %s",k,v) end end
  u=  #t>1 and lib.map(t,f or tostring) or lib.sort(lib.kap(t,key))
  return (t._is or "").."{"..table.concat(u," ").."}" end

--> chat(t:tab):t -> Print table (as string). Return `t`.
function lib.chat(t) print(lib.cat(t)); return t end
-- ### Settings

--> opts(x:str) :tab -> Parse `str` for lines with `--`; then pull keys+defaults. 
function lib.opts(x)
  local t = {}
  x:gsub("\n  ([-][^%s]+)[%s]+([-][-]([^%s]+))[^\n]*%s([^%s]+)",
           function(f1,f2,k,x) t[k] = lib.thing(x) end)
  t._HELP = x
  return t end

--> cli(t:tab) :tab -> For keys in `t`, look for updates on command-line. 
-- Things with boolean defaults are flipped via `--flag`. 
-- Other keys need `--flag value`.  Print the help
-- (if `-h` appears on command line). Return a table with setting `key`s and
-- `value`s. IMPORTANT NOTE: this function alters-in-place the table `t`
-- that is passed in-- which means that it alters settings for anything pointing
-- to `t`.
function lib.cli(t)
  for key,x in pairs(t) do 
    x = tostring(x)
    local long, short = "--"..key, "-"..key:sub(1,1)
    for n,flag in ipairs(arg) do 
      if flag==short or flag==long then
        x = x=="false" and "true" or x=="true" and "false" or arg[n+1] 
        t[key] = lib.thing(x) end end end
  if t.help then os.exit(print(t._HELP:gsub("[%u][%u%d]+","\27[1;32m%1\27[0m"),"")) end 
  return t end
-- ### Tests

--> on(opts:tab, tests:[fun]) -> Run some tests.
-- If  `opt.go=="all"`, then run all tests, sorted on their name.
-- Before each test, reset random seed and the options `opts.
function lib.on(opts,tests)
  local fails, old = 0, {}
  for k,v in pairs(opts) do old[k]=v end
  local t=opts.go=="all" and lib.kap(tests,function(k,_) return k end) or {opts.go}
  for _,txt in pairs(lib.sort(t)) do
    local fun = tests[txt]
    if type(fun)=="function" then
      for k,v in pairs(old) do opts[k]=v end -- reset opts to default
      math.randomseed(opts.seed or 10019)    -- reset seed to default
      print(">> ",txt)
      local out = fun()
      if out ~= true then fails=fails+1
                          print(lib.fmt("FAIL: [%s] %s",txt,out or "")) end end end
  lib.rogues()
  os.exit(fails) end -- if fails==0 then our return code to the OS will be zero.
-- ### Objects

--> obj(name:str, fun:fun):object -> Return a klass `name` with constructor `fun`.
-- Add a unique `id` and a `tosting` method (that uses `cat` (above).
local _id = 0
function lib.obj(name,fun,    t,new,x)
  function new(kl,...) _id=_id+1; x=setmetatable({_id=_id},kl);fun(x,...); return x end 
  t = {__tostring=lib.cat,_is=name}; t.__index=t
  return setmetatable(t, {__call=new}) end
-----
-- ### Return

return lib
