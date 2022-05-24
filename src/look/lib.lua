-- vim: ts=2 sw=2 et : 
-- LIB.LUA : misc support code.
-- (c) 2022 Tim Menzies.  BSD-2 license.
local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local fmt =string.format
local rand=math.random
local big = 1E32

local function any(t)       return t[math.random(#t)] end
local function many(t,n, u) u={};for j=1,n do u[1+#u]=any(t) end; return u end
local function lt(x)        return function(a,b) return a[x]<b[x] end end
local function gt(x)        return function(a,b) return a[x]>b[x] end end
local function push(t,x)    t[1+#t]=x; return x end
local function sort(t,f)    table.sort(t,f); return t end
local function map(t,f,  u) u={}; for k,v in pairs(t) do u[1+#u]=f(v) end
                            return u end

local function splice(t,i,j,k,     u) 
  u={}; for n=(i or 1)//1, (j or #t)//1,(k or 1)//1 do 
          u[1+#u] = t[n] end return u end

local function per(t,p, i) i=p*#t//1; return t[math.max(1,math.min(#t,i))] end

local function shuffle(t,    j) 
  for i = #t, 2, -1 do j=rand(i); t[i],t[j] = t[j],t[i] end
  return t end

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

local function cli(d,help)
  d = d or {}
  for key,x in pairs(d) do
    x = tostring(x)
    for n,flag in ipairs(arg) do 
      if flag==("-"..key:sub(1,1)) or flag==("--"..key) then
        x = x=="false" and"true" or x=="true" and"false" or arg[n+1] end end 
    d[key] = tothing(x) end
  if d.help then return os.exit(print(
     help:gsub("[%u][%u%d]+", "\27[31m%1\27[0m")
         :gsub("\"[^\"]+\"", "\27[32m%1\27[0m")
         :gsub("(%s)([-][-]?[^%s]+)(%s)","%1\27[33m%2\27[0m%3"),"")) end 
  return d end 

local function o(t,    u)
  if #t>0 then return "{"..table.concat(map(t,tostring)," ").."}" end
  u={}; for k,v in pairs(t) do u[1+#u] = fmt(":%s %s",k,v) end
  return (t.is or "").."{"..table.concat(sort(u)," ").."}" end 

local function oo(x) print(o(x)); return x end

local function is(name,    t,new)
  function new(kl,...) local x=setmetatable({},kl); kl.new(x,...); return x end 
  t = {__tostring=o, is=name or ""}; t.__index=t
  return setmetatable(t, {__call=new}) end

local function main(funs,settings)
  local defaults, names, fails = {}, {}, 0
  for k,f in pairs(funs) do 
    if type(f)=="function" then push(names,k) end end 
  for k,v in pairs(settings) do 
    defaults[k]=v end
  if funs[settings.go] then 
    names={settings.go} end
  for _,one in pairs(sort(names))  do         -- for all we want to do
    for k,v in pairs(defaults) do 
      settings[k]=v end                      -- reset the settings to defaults
    math.randomseed(settings.seed or 10019)  -- reset random number seed
    io.stderr:write(".")
    local status = funs[one]()               -- run demo
    if status ~= true then
      print("-- Error",one,status) 
      fails = fails + 1 end end              -- update fails
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end 
  os.exit(fails) end

return {any=any, big=big, cli=cli, csv=csv, fmt=fmt, gt=gt,is=is, lt=lt, 
        oo=oo, o=o, main=main, many=many, map=map, per=per, push=push, rand=rand, 
        shuffle=shuffle, sort=sort, splice=splice, tothing=tothing}
