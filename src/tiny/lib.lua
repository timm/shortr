local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local rogues=function()
        for k,v in pairs(_ENV) do 
          if not b4[k] then print("?",k,type(v)) end end end
  
local fmt =string.format
local rand=math.random
local big = 1E32

local function lt(x)        return function(a,b) return a[x]<b[z] end end
local function push(t,x)    t[1+#t]=x; return x end
local function sort(t,f)    table.sort(t,f); return t end
local function map(t,f, u) 
  u={};for k,v in pairs(t) do u[1+#u]=f(v) end;return u end

local function tothing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x  end

local function csv(csvfile) 
  csvfile = io.input(csvfile)
  return function(line, t) 
    line=io.read()
    if not line then io.close(csvfile) else
      t={}; for x in line:gmatch("([^,]+)") do t[1+#t]=tothing(x) end
      return t end end end 

local function o(t,    u)
  if #t>0 then return "{"..table.concat(map(t,tostring)," ").."}" end
  u={}; for k,v in pairs(t) do u[1+#u] = fmt(":%s %s",k,v) end
  return (t.is or "").."{"..table.concat(sort(u)," ").."}" end 
local function oo(x) print(o(x)) end

local function is(name,    t,new)
  function new(kl,...) local x=setmetatable({},kl); kl.new(x,...); return x end 
  t = {__tostring=M.o, is=name or ""}; t.__index=t
  return setmetatable(t, {__call=new}) end


return {big=big,csv=csv,fmt=fmt,is=is,lt=lt,oo=oo,o=o,map=map,push=push,
        rand-rand, rogues=rogues, sort=sort, tothing=tothing,
}

