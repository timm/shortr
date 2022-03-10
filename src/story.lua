---     ___              __        
---    /\_ \      __    /\ \       
---    \//\ \    /\_\   \ \ \____  
---      \ \ \   \/\ \   \ \ '__`\ 
---       \_\ \_  \ \ \   \ \ \L\ \
---       /\____\  \ \_\   \ \_,__/
---       \/____/   \/_/    \/___/ 
-- ## Just a few standard tricks
--
--
local _={}

---     _ _  _ _|_|_  _                                                  
---    | | |(_| | | |_\

_.r=math.random
function _.ish(x,y,z)  return math.abs(y -x ) < z end 
function _.cosine(a,b,c) 
  return math.max(0,math.min(1, (a^2+c^2-b^2)/(2*c+1E-32))) end

---    |. __|_ _                                                        
---    ||_\ | _\

function _.any(a)        return a[ math.random(#a) ] end
function _.firsts(a,b)   return a[1] < b[1] end
function _.last(a)       return a[ #a ] end
function _.many(a,n,  u) u={}; for j=1,n do u[1+#u] =_.any(a) end; return u end
function _.per(a,p)      return a[ (p*#a)//1 ] end
function _.pop(a)        return table.remove(a) end
function _.push(t,x)     t[1 + #t] = x; return x end
function _.sort(t,f)     table.sort(t,f); return t end

function _.map(t,f, u)   
  u={};for k,v in pairs(t) do u[1+#u]=f(v) end;return u end

function _.sum(t,f, n) 
  f = f or function(x) return x end
  n=0; for k,v in pairs(t) do n = n + f(v) end; return n end

function _.shuffle(t,   j)
  for i=#t,2,-1 do j=math.random(i); t[i],t[j]=t[j],t[i] end; return t end

---     __|_ _. _  _   '~)  _|_|_ . _  _                               
---    _\ | | || |(_|   /_   | | ||| |(_|
---                _|                  _|

function _.thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return tonumber(x) or x end

function _.things(file,      x)
  local function f(x,  t)
    t={}; for y in x:gmatch("([^,]+)") do t[1+#t] = _.thing(y) end; return t end
  file = io.input(file)
  return function()
    x=io.read(); if x then return f(x) else io.close(file) end end end

---    _|_|_ . _  _   '~)   __|_ _. _  _                               
---     | | ||| |(_|   /_  _\ | | || |(_|
---               _|                   _|

_fmt = string.format

function _.oo(t) print(_.o(t)) end

function _.o(t,  seen, u)  
  if type(t)~="table" then return tostring(t) end
  seen = seen or {}
  if seen[t] then return "..." end
  seen[t] = t
  local function show1(x) return _.o(x, seen) end
  local function show2(k) return _.fmt(":%s %s",k, _.o(t[k],seen)) end
  u = #t>0 and _.map(t,show1) or _.map(_slots(t),show2)
  return (t._is or "").."{"..table.concat(u," ").."}" end

function _.slots(t, u)
  local function public(k) return tostring(k):sub(1,1) ~= "_" end
  u={};for k,v in pairs(t) do if public(k) then u[1+#u]=k end end
  return sort(u) end

function _.rnds(t,f) return map(t, function(x) return _rnd(x,f) end) end
function _.rnd(x,f) 
  f = not f and "%s" or number and fmt("%%%sf",f) or f
  return fmt(type(x)=="number" and (x~=x//1 and f) or "%s",x) end

---    |_  _ | _   _|_ _   _|_  '~)   _ _ _|__|_. _  _  _             --!the
---    | |(/_||_)   | (/_>< |    /_  _\(/_ |  | || |(_|_\
---           |                                      _|  

function _.settings(help,    d)
  d={}
  help:gsub("\n  ([-]([^%s]+))[%s]+(-[^%s]+)[^\n]*%s([^%s]+)",
    function(long,key,short,x)
      for n,flag in ipairs(arg) do 
        if flag==short or flag==long then
          x = x=="false" and true or x=="true" and "false" or arg[n+1] end end 
       d[key] = x==true and true or thing(x) end)
  if d.help then print(help) end
  return d end

---     _ _  _ _|_ _ _ |                                               --!cntr
---    (_(_)| | | | (_)|
                 
GO, NO = {fails=0}, {}
function _.ok(test,msg)
  print(test and "      PASS: "or "      FAIL: ",msg or "") 
  if not test then 
    GO.fails = GO.fails+1 
    if the.dump then assert(test,msg) end end end

function _.main(todo,seed)
  for k,one in pairs(todo=="all" and slots(GO) or {todo}) do
    if k ~= "main" and type(GO[one]) == "function" then
      math.randomseed(seed)
      print(fmt("#%s",one))
      GO[one]() end end 
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end  end

---     _ |_  . _  __|_ _                                               --!obj
---    (_)|_) |(/_(_ | _\
---          L|          

new = setmetatable
function _.class(s,   t)
  t={__tostring=o,_is=s or ""}; t.__index=t
  return new(t, {__call=function(_,...) return t.new(_,...) end}) end

return _
