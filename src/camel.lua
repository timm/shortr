local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local the = {
             bins  = 16, 
             cohen = .35,
             file  = "../etc/data/auto93.csv",
             how   = "up",
             min   = .5
            }

local big = 1E32
local fmt = string.format

local function lt(x)       return function(a,b) return a[x] < b[x] end end 
local function push(t,x)   t[1+#t]=x; return x end
local function sort(t,f)   table.sort(t,f); return t end
local function map(t,f, u) u={};for k,v in pairs(t) do u[1+#u]=f(v) end; return u end
local function copy(t,   u)
  if type(t) ~= "table" then return t end
  u={};for k,v in pairs(t) do u[copy(k)]=copy(v) end
  return setmetatable(u,getmetatable(t)) end

local function string2thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x  end

local function csv(csvfile) 
  csvfile = io.input(csvfile)
  return function(line, row) 
    line=io.read()
    if not line then io.close(csvfile) else
      row={}; for x in line:gmatch("([^,]+)") do push(row,string2thing(x)) end
      return row end end end 

local function o(t,    u)
  if #t>0 then return "{"..table.concat(map(t,tostring)," ").."}" else
    u={}; for k,v in pairs(t) do u[1+#u] = fmt(":%s %s",k,v) end
    return (t.is or "").."{"..table.concat(sort(u)," ").."}" end end
local function oo(t) print(o(t)) end

local function cli(d)
  for slot,x in pairs(d) do
    x = tostring(x)
    for n,flag in ipairs(arg) do 
      if flag=="--"..slot or flag=="-"..slot:sub(1,1) then
        x = x=="false" and"true" or x=="true" and"false" or arg[n+1] end end 
    d[slot] = string2thing(x) end 
  return d end

the = cli(the)

--------------------------------------------------------------------------------
local function is(name,    t,new)
  function new(kl,...) local x=setmetatable({},kl); kl.new(x,...); return x end 
  t = {__tostring=o, is=name or ""}; t.__index=t
  return setmetatable(t, {__call=new}) end

local NUM,SYM,EGS = is"NUM", is"SYM", is"EGS"

local function use(s)    return s ~= "?" end
local function skip(s)   return s:find":$" end
local function klassp(s) return s:find"!$" end
local function nump(s)   return s:find"^[A-Z]" end
local function goalp(s)  return s:find"[!+-]$" end
local function weight(s) return s:find"-$" and -1 or 1 end

function SYM.new(i,at,name) 
  i.n,i.txt,i.at,i.all = 0,txt or "",at or 0,{}  end

function SYM.add(i,x)
  if use(x) then i.n = i.n+1; i.all[x]= 1+(i.all[x] or 0) end end

function NUM.new(i,at,txt) 
  i.n,i.mu,i.txt,i.at = 0,0,txt or "",at or 0
  i.w,i.lo,i.hi       = i.txt:find"-$" and -1 or 1,big,-big end

function NUM.add(i,x,    d)  
  if use(x) then 
    i.n  = i.n+1
    d    = i.mu - x
    i.mu = i.mu+d/i.n
    i.lo = math.min(x, i.lo)
    i.hi = math.max(x, i.hi) end end

function NUM.norm(i,x) 
  return use(x) and (i.hi-i.lo<1E-9 and 0 or (x-i.lo)/(i.hi-i.lo)) end

function EGS.order(i)
  sort(i.rows, function(r1,r2) 
                 local s1,s2,e,y,a,b = 0,0,math.exp(1),i.y
                 for _,col in pairs(y) do
                   a,b = col:norm(r1[col.at]), col:norm(r2[col.at])
                   s1 = s1 - e^(col.w * (a - b) / #y)
                   s2 = s2 - e^(col.w * (b - a) / #y) end
                 return s1/#y < s2/#y end) 
  return i end

function EGS.new(i,names)
  i.rows, i.names, i.all, i.y = {}, names, {}, {}
  for at,txt in pairs(names) do 
    local col = push(i.all, (nump(txt) and NUM or SYM)(at,txt))
    if goalp(txt) then push(i.y, col) end end end

function EGS.add(i, row)
  push(i.rows,row)
  for _,col in pairs(i.all) do col:add(row[col.at]) end end

local function egs(f, i)
  for row in csv(f or the.file) do 
    if i then i:add(row) else i=EGS(row) end end
  return i:order() end

local x=egs()
for i=1,5 do oo(x.rows[i]) end; print""
for i=#x.rows-5,#x.rows do oo(x.rows[i]) end
for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end
