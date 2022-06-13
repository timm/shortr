big  = math.huge
fmt  = string.format
R    = math.random

function push(t,x)      t[1+#t]=x; return x end
function sort(t,f)      table.sort(t,f); return t end
function map(t,f,  u)   u={}; for k,v in pairs(t) do u[1+#u]=f(v) end; return u end
function sum(t,f,  n)   n=0;  for k,v in pairs(t) do n = n + f(v) end; return n end
function per(t,p)       p=p*#t//1; return t[math.max(1,math.min(#t,p))] end
function lt(x)          return function(a,b) return a[x] < b[x] end end

function argmax(t,f)
  local arg, max = nil, -big
  for key,x in pairs(t) do local tmp=f(x);if tmp>max then arg,max=key,tmp end end
  return arg end


function o(t) 
  if #t>0 then return "{" .. table.concat(map(t,tostring)," ") .."}" end
  local u={}; for k,v in pairs(t) do u[1+#u] = fmt(":%s %s",k,v) end
  return "{"..table.concat(sort(u)," ").."}" end 

function oo(t) print(o(t)); return t end

function atom(str) 
  str = str:match"^%s*(.-)%s*$"
  if str=="true" then return true elseif str=="false" then return false end
  return math.tointeger(str) or tonumber(str) or str  end

function csv(file) 
  file = io.input(file)
  return function(s, t) 
    s=io.read()
    if not s then io.close(file) else
      t={}; for x in s:gmatch("([^,]+)") do t[1+#t]=atom(x) end; return t end end end 

function splice( t, i, j, k,    u) 
  u={};for n=(i or 1)//1,(j or #t)//1,(k or 1)//1 do u[1+#u]=t[n] end;return u end
