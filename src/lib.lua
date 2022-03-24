local _={}

---     _ _  _ _|_|_  _
---    | | |(_| | | |_\

function _.per(t,p) return t[ (p or .5)*#t//1 ] end 

function _.ent(t) 
  local n=0; for _,m in pairs(t) do n = n+m end
  local e=0; for _,m in pairs(t) do if m>0 then e= e+m/n*math.log(m/n,2) end end
  return -e,n end

function _.norm(lo,hi,x) return math.abs(hi-lo)<1E-9 and 0 or (x-lo)/(hi-lo) end

---     _ |_  _   _ | 
---    (_ | |(/ _(_ |<

function _.ish(x,y,z) return math.abs(x-y) <= (z or 0.001) end

---     |`.|_|_ _  _. _  _ 
---    ~|~|| | (/_| || |(_|
---                      _|
              
function _.inc(f,a,n)      f=f or{};f[a]=(f[a] or 0) + (n or 1)    return f end
function _.inc2(f,a,b,n)   f=f or{};f[a]=_.inc(f[a]  or {},b,n); return f end
function _.inc3(f,a,b,c,n) f=f or{};f[a]=_.inc2(f[a] or{},b,c,n);return f end

function _.has(f,a)      return f[a]                        or 0 end
function _.has2(f,a,b)   return f[a] and _.has( f[a],b)   or 0 end
function _.has3(f,a,b,c) return f[a] and _.has2(f[a],b,c) or 0 end

---    |. __|_ _
---    ||_\ | _\

_.unpack = table.unpack

function _.push(t,x) t[1 + #t] = x; return x end

function _.powerset(s)
  local function aux(s)
    local t = {{}}
    for i = 1, #s do
      for j = 1, #t do
        t[#t+1] = {s[i], _.unpack(t[j])} end end
    return t end
  return _.sort(aux(s), function(a,b) return #a < #b end) end

---     |`.|_|_ _  _. _  _ 
---    ~|~|| | (/_| || |(_|
---                      _|

function _.map(t, f, u) 
  u={}; for k,v in pairs(t) do u[1+#u]=f(v) end; return u end
function _.collect(t,f,u) 
  u={}; for k,v in pairs(t) do u[k]=f(k,v) end; return u end
function _.copy(t,   u)
  if type(t) ~= "table" then return t end
  u={}; for k,v in pairs(t) do u[_.copy(k)] = _.copy(v) end; return u end

---     _ _  __|_. _  _ 
---    _\(_)|  | || |(_|
---                   _|

function _.sort(t,f) table.sort(t,f); return t end

function _.upx(a,b)   return a.x < b.x end
function _.up1(a,b)   return a[1] < b[1] end
function _.down1(a,b) return a[1] > b[1] end

function _.slots(t, u)
  local function public(k) return tostring(k):sub(1,1) ~= "_" end
  u={};for k,v in pairs(t) do if public(k) then u[1+#u]=k end end
  return _.sort(u) end

---     _ _ | _  __|_. _  _ 
---    _\(/_|(/_(_ | |(_)| |
                     
function _.any(a,lo,hi) 
  lo,hi = lo or 1, hi or #a; return a[ (lo+(hi-lo)*math.random())//1 ] end

function _.many(a,n,lo,hi,  u) 
  u={}; for j=1,n do _.push(u, _.any(a,lo,hi)) end; return u end

function _.slice(a,lo,hi,    u)
  u,lo,hi = {},lo or 1,hi or #a; for j=lo,hi do u[1+#u]=a[j] end; return u end

---     __|_ _. _  _   '~)  _|_|_ . _  _ 
---    _\ | | || |(_|   /_   | | ||| |(_|
---                _|                  _|

function _.words(s,sep,   t)
  sep="([^" .. (sep or ",")  .. "]+)"
  t={}; for y in s:gmatch(sep) do t[1+#t] = y end; return t end

function _.things(s) return _.map(_.words(s), thing) end 

function _.thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return tonumber(x) or x end 

function _.items(src,f)
  local function file()
    src,f = io.input(src),f or _.things
    return function() x=io.read()
             print(6000,f)
             if x then return f(x) else io.close(src) end end end 
  local function tbl(   x)
    print(300)
    x,f = 0, f or function(z) return z end
    return function() if x< #src then x=x+1; return f(src[x]) end end end 
  if src then
    return type(src) == "string" and file() or tbl() end end

---    _|_|_ . _  _  _  '~)   __|_ _. _  _ 
---     | | ||| |(_|_\   /_  _\ | | || |(_|
---               _|                     _|

_.fmt = string.format

function _.oo(t) print(_.o(t)) end

function _.o(t,  seen, u)  
  if type(t)~="table" then return tostring(t) end
  seen = seen or {}
  if seen[t] then return "..." end
  seen[t] = t
  local function show1(x) return _.o(x, seen) end
  local function show2(k) return _.fmt(":%s %s",k, _.o(t[k],seen)) end
  u = #t>0 and _.map(t,show1) or _.map(_.slots(t),show2)
  return (t._is or "").."{"..table.concat(u," ").."}" end

function _.dent(t,  seen,pre)  
  pre,seen = pre or "", seen or {}
  if seen[t] then t= "..." end
  if type(t)~="table" then return print(pre .. tostring(t)) end
  seen[t]=t
  for _,k in pairs(_.slots(t)) do
    local v = t[k]
    local after = type(v)=="table" and "\n" or "\t"
    io.write(pre,":",k,after)
    if   type(v)=="table" 
    then _.dent(v,seen,"|  "..pre) 
    else print(v) end end end

function _.rnds(t,f) 
  return _.map(t, function(x) return _.rnd(x,f) end) end

function _.rnd(x,f) 
  return _.fmt(type(x)=="number" and (x~=x//1 and f or "%5.2f") or "%s",x) end

return _
