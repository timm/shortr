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
function obj(name,fun,    t,new)
  function new(kl,...) 
    _id = _id + 1
    local x=setmetatable({id=_id},kl); fun(x,...); return x end 
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

local Num=obj("Num", function(i,at,txt)
  i.n,i.at,i.txt,i.kept = 0,at or 0 ,txt or "",{} 
  i.names,i.ok = 256,true end)

local Sym=obj("Sym", function(i,at,txt)
  i.n,i.at,i.txt,i.kept = 0,at or 0,txt or "",{} end)

function init(at,txt)
  local i=(name:find"^[A-Z]" and Num or Sym)(at,txt)
  i.w = i.txt:find"-$" and -1 or 1
  i.ignorep = i.txt:find":$"
  return i  end

local Rows=obj("Row", function(i,row) i.rows={}; i.cols=nil; i.categories={} end)
function Rows.add(i,row)
  rs.kepts = rs.cols and maps(r.kepts,row,update) or i:categorize(kap(row,init) end)

function Rows.categorize(i,cols)
  for _,col in pairs(cols) do if not col.ignorep then 
     push(col.txt:find"[!+-]$" and i.categories.y or i.categories.y, col) end end 
  return end

function make(f,rows) 
  local function make1(row) if rows then rows:add(row) else rows=Rows(row) end
  if type(src)=="table" then map(rows,make1) else csv(src,make1) end
  return rows end
