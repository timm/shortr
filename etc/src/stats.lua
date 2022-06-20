function map(t,f,  u) u={}; for _,v in pairs(t) do u[1+#u]=f(v)   end; return u end
function kap(t,f,  u) u={}; for k,v in pairs(t) do u[k]   =f(k,v) end; return u end

function maps(t,u,f,  v) v={}; for k,v in pairs(t) do v[1+#v]=f(v,u[k])   end; return v end
function kaps(t,u,f,  v) v={}; for k,v in pairs(t) do v[k]   =f(k,v,u[k]) end; return v end

function sort(t,f) table.sort(t,f); return t end

function cat(t,f)
  local function key(k,v) return fmt(":%s %s",k,v) end
  t=  #t>1 and  map(t,f or tostring) or sort(kap(t,f or key))
  return "{"..table.concat(t," ").."}" end

function chat(t) print(cat(t)); return t end

local _id = 0
function obj(name,    t,new)
  function new(kl,...) 
    _id = _id + 1
    local x=setmetatable({id=_id},kl); kl.NEW(x,...); return x end 
  t = {__tostring=cat, is=name or ""}; t.__index=t
  return setmetatable(t, {__call=new}) end

function thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x end 

function csv(file, fun)
  local file = io.input(file)
  local line = io.read()
  while line do
    local t={}; for x in line:gmatch("([^,]+)") do t[1+#t]=thing(x) end; fun(t) 
    line = io.read() end
  io.close(file) end

local Num=obj"Num"
function Num.NEW(i,at,txt)
  i.n,i.at,i.txt,i.kept = 0,at,txt,{} 
  i.names,i.ok = 256,true end

function init(at,txt)
  local new=(name:find"^[A-Z]" and Num or Sym)(at,txt)
  new.w = txt:find"-$" and -1 or 1
  new.ignorep = txt:find":$"
  return new end

csv("../../data/auto93.csv",function(row) 
  data = data and maps2(data,row,update) or map2(row,init) end
