local lib={}

lib.big = math.huge
lib.fmt = string.format
lib.R   = math.random

function lib.push(t,x)      t[1+#t]=x; return x end
function lib.sort(t,f)      table.sort(t,f); return t end
function lib.map(t,f,  u)   u={}; for k,v in pairs(t) do u[1+#u]=f(v) end; return u end
function lib.sum(t,f,  n)   n=0;  for k,v in pairs(t) do n = n + f(v) end; return n end
function lib.per(t,p)       p=p*#t//1; return t[math.max(1,math.min(#t,p))] end
function lib.lt(x)          return function(a,b) return a[x] < b[x] end end

function lib.argmax(t,f)
  local arg, max = nil, -lib.big
  for key,x in pairs(t) do local tmp=f(x);if tmp>max then arg,max=key,tmp end end
  return arg end

function lib.o(t) 
  if type(t) ~= "table" then return tostring(t) end
  if #t>0 then return "{" .. table.concat(lib.map(t,tostring)," ") .."}" end
  local u={}; for k,v in pairs(t) do u[1+#u] = lib.fmt(":%s %s",k,v) end
  return "{"..table.concat(lib.sort(u)," ").."}" end 

function lib.oo(t) print(lib.o(t)); return t end

function lib.atom(str) 
  str = str:match"^%s*(.-)%s*$"
  if str=="true" then return true elseif str=="false" then return false end
  return math.tointeger(str) or tonumber(str) or str  end

function lib.csv(file) 
  file = io.input(file)
  return function(s, t) 
    s=io.read()
    if not s then io.close(file) else
      t={};for x in s:gmatch("([^,]+)")do t[1+#t]=lib.atom(x)end;return t end end end 

function lib.splice( t, i, j, k,    u) 
  u={};for n=(i or 1)//1,(j or #t)//1,(k or 1)//1 do u[1+#u]=t[n] end;return u end

return lib
