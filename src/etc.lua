-- vim: ts=2 sw=2 et : 
-- etc.lua : misc support code.
-- (c) 2022 Tim Menzies.  Usage of the works is permitted provided that this
-- instrument is retained with the works, so that any entity that uses the works
-- is notified of this instrument. DISCLAIMER:THE WORKS ARE WITHOUT WARRANTY.  

local M={}
M.b4={}; for k,_ in pairs(_ENV) do M.b4[k]=k end 

M.big =1E32
M.fmt =string.format
M.rand=math.random

M.lt  =function(x)      return function(a,b) return a[x] < b[x] end end 
M.map =function(t,f, u) u={};for k,v in pairs(t) do u[1+#u]=f(v) end;return u end
M.push=function(t,x)    t[1+#t]=x; return x end
M.sort=function(t,f)    table.sort(t,f); return t end
M.any= function(t)      return t[math.random(#t)] end
M.per= function(t,p, i) i=p*#t//1; return t[math.max(1,math.min(#t,i))] end

function M.cli(d,help)
  for key,x in pairs(d) do
    x = tostring(x)
    for n,flag in ipairs(arg) do 
      if flag==("-"..key:sub(1,1)) or flag==("--"..key) then
        x = x=="false" and"true" or x=="true" and"false" or arg[n+1] end end 
      d[key] = M.tothing(x) end 
  if d.help then  
     os.exit(
       print(
         help:gsub("%u%u+", "\27[1;31m%1\27[0m")
             :gsub("(%s)([-][-]?[^%s]+)(%s)","%1\27[1;36m%2\27[0m%3"),"")) end
  return d end

function M.csv(csvfile) 
  csvfile = io.input(csvfile)
  return function(line, t) 
    line=io.read()
    if not line then io.close(csvfile) else
      t={}; for x in line:gmatch("([^,]+)") do M.push(t,M.tothing(x)) end
      return t end end end 

function M.oo(t) print(M.o(t)) end
function M.o(t,    u)
  if #t>0 then return "{"..table.concat(M.map(t,tostring)," ").."}" else
    u={}; for k,v in pairs(t) do u[1+#u] = M.fmt(":%s %s",k,v) end
    return (t.is or "").."{"..table.concat(M.sort(u)," ").."}" end end

function M.splice(t,i,j,k,     u) 
  u={}; for n=(i or 1), (j or #t),(k or 1) do u[1+#u] = t[n] end return u end

function M.tothing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x  end

function M.is(name,    t,new)
  function new(kl,...) local x=setmetatable({},kl); kl.new(x,...); return x end 
  t = {__tostring=M.o, is=name or ""}; t.__index=t
  return setmetatable(t, {__call=new}) end

return M
