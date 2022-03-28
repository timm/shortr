local lib={}

---     _ _  _ _|_|_  _
---    | | |(_| | | |_\

function lib.per(t,p) return t[ (p or .5)*#t//1 ] end 

function lib.ent(t) 
  local n=0; for _,m in pairs(t) do n = n+m end
  local e=0; for _,m in pairs(t) do if m>0 then e= e+m/n*math.log(m/n,2) end end
  return -e,n end

function lib.norm(lo,hi,x) return math.abs(hi-lo)<1E-9 and 0 or (x-lo)/(hi-lo) end

function lib.cosine(a,b,c) 
  return math.max(0,math.min(1, (a^2+c^2-b^2)/(2*c+1E-32))) end

---     _ |_  _   _ | 
---    (_ | |(/ _(_ |<

function lib.ish(x,y,z) return math.abs(x-y) <= (z or 0.001) end

---     |`.|_|_ _  _. _  _ 
---    ~|~|| | (/_| || |(_|
---                      _|
              
function lib.inc(f,a,n)      f=f or{};f[a]=(f[a] or 0) + (n or 1)    return f end
function lib.inc2(f,a,b,n)   f=f or{};f[a]=lib.inc(f[a]  or {},b,n); return f end
function lib.inc3(f,a,b,c,n) f=f or{};f[a]=lib.inc2(f[a] or{},b,c,n);return f end

function lib.has(f,a)      return f[a]                        or 0 end
function lib.has2(f,a,b)   return f[a] and lib.has( f[a],b)   or 0 end
function lib.has3(f,a,b,c) return f[a] and lib.has2(f[a],b,c) or 0 end

---    |. __|_ _
---    ||_\ | _\

lib.unpack = table.unpack

function lib.push(t,x) t[1 + #t] = x; return x end

function lib.powerset(s)
  local function aux(s)
    local t = {{}}
    for i = 1, #s do
      for j = 1, #t do
        t[#t+1] = {s[i], lib.unpack(t[j])} end end
    return t end
  return lib.sort(aux(s), function(a,b) return #a < #b end) end

---     |`.|_|_ _  _. _  _ 
---    ~|~|| | (/_| || |(_|
---                      _|

function lib.map(t, f, u) 
  u={}; for k,v in pairs(t) do u[1+#u]=f(v) end; return u end
function lib.collect(t,f,u) 
  u={}; for k,v in pairs(t) do u[k]=f(k,v) end; return u end
function lib.copy(t,   u)
  if type(t) ~= "table" then return t end
  u={}; for k,v in pairs(t) do u[lib.copy(k)] = lib.copy(v) end; return u end

---     _ _  __|_. _  _ 
---    _\(_)|  | || |(_|
---                   _|

function lib.sort(t,f) table.sort(t,f); return t end

function lib.upx(a,b)   return a.x < b.x end
function lib.up1(a,b)   return a[1] < b[1] end
function lib.down1(a,b) return a[1] > b[1] end

function lib.slots(t, u)
  local function public(k) return tostring(k):sub(1,1) ~= "_" end
  u={};for k,v in pairs(t) do if public(k) then u[1+#u]=k end end
  return lib.sort(u) end

---      _  _|_   _.  ._  _|_         ._  
---     _>   |_  (_|  |    |_    |_|  |_) 
---                                   |   

function lib.settings(help)
  local d,used = {},{}
  help:gsub("\n  ([-]([^%s]+))[%s]+(-[^%s]+)[^\n]*%s([^%s]+)",
    function(long,key,short,x)
      assert(not used[short], "repeated short flag ["..short.."]")
      used[short]=short
      for n,flag in ipairs(arg) do 
        if flag==short or flag==long then
          x = x=="false" and true or x=="true" and "false" or arg[n+1] end end 
      d[key] = lib.coerce(s) end)
  return d end

function lib.onTheGo(the,go,b4,           old,todos)
  the,go = (the or {}),(go or {})
  old={}; for k,v in pairs(the) do old[k]=v end
  todos = old.todo == "all" and slots(go) or {old.todo}
  for _,todo in pairs(todos) do
    for k,v in pairs(old) do the[k]=v end
    math.randomseed(the.seed or 10019)
    if go[todo] then print("\n"..todo); go[todo]() end end 
  if b4 then
    for k,v in pairs(_ENV) do 
       if not b4[k] then print("?",k,type(v)) end end end end 

---     _ _ | _  __|_. _  _ 
---    _\(/_|(/_(_ | |(_)| |
                     
function lib.any(a,lo,hi) 
  lo,hi = lo or 1, hi or #a; return a[ (lo+(hi-lo)*math.random())//1 ] end

function lib.many(a,n,lo,hi,  u) 
  u={}; for j=1,n do lib.push(u, lib.any(a,lo,hi)) end; return u end

function lib.slice(a,lo,hi,    u)
  u,lo,hi = {},lo or 1,hi or #a; for j=lo,hi do u[1+#u]=a[j] end; return u end

---     __|_ _. _  _   '~)  _|_|_ . _  _ 
---    _\ | | || |(_|   /_   | | ||| |(_|
---                _|                  _|

function lib.coerce(s) 
  return type(s)~="string" and s or math.tointeger(s) or tonumber(s) or s end

function lib.words(s,sep,   t)
  sep="([^" .. (sep or ",")  .. "]+)"
  t={}; for y in s:gmatch(sep) do t[1+#t] = y end; return t end

function lib.things(s) 
  return lib.map(lib.words(s), lib.thing) end 

function lib.thing(x)
  if type(x) ~= "string" then return x end
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return lib.coerce(x) end

function lib.items(src,f)
  local function file(f)
    src,f = io.input(src),(f or lib.things)
    return function(x) x=io.read()
             if x then return f(x) else io.close(src) end end end 
  local function tbl(   x)
    x,f = 0, f or function(z) return z end
    return function() if x< #src then x=x+1; return f(src[x]) end end end 
  if src then
    return type(src) == "string" and file(f) or tbl() end end

---    _|_|_ . _  _  _  '~)   __|_ _. _  _ 
---     | | ||| |(_|_\   /_  _\ | | || |(_|
---               _|                     _|

lib.fmt = string.format

function lib.oo(t) print(lib.o(t)) end

function lib.o(t,  seen, u)  
  if type(t)~="table" then return tostring(t) end
  seen = seen or {}
  if seen[t] then return "..." end
  seen[t] = t
  local function show1(x) return lib.o(x, seen) end
  local function show2(k) return lib.fmt(":%s %s",k, lib.o(t[k],seen)) end
  u = #t>0 and lib.map(t,show1) or lib.map(lib.slots(t),show2)
  return (t._is or "").."{"..table.concat(u," ").."}" end

function lib.dent(t,  seen,pre)  
  pre,seen = pre or "", seen or {}
  if seen[t] then t= "..." end
  if type(t)~="table" then return print(pre .. tostring(t)) end
  seen[t]=t
  for key,k in pairs(lib.slots(t)) do
    local v = t[k]
    io.write(lib.fmt("%s:%s%s",pre,k, type(v)=="table" and "\n" or " "))
    if   type(v)=="table" 
    then lib.dent(v,seen,"|  "..pre) 
    else print(v) end end end

function lib.rnds(t,f) 
  return lib.map(t, function(x) return lib.rnd(x,f) end) end

function lib.rnd(x,f) 
  return lib.fmt(type(x)=="number" and (x~=x//1 and f or "%5.2f") or "%s",x) end

return lib
