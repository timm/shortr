the={seed=10019}
fmt=string.format
same=function(x) return x end
function sort(t,f) table.sort(t,f); return t end

function map(t,f,  u) u={}; for _,v in pairs(t) do u[1+#u]=f(v)   end; return u end
function kap(t,f,  u) u={}; for k,v in pairs(t) do u[1+#u]=f(k,v) end; return u end

function maps(t,u,f,  v) v={}; for k,v in pairs(t) do v[1+#v]=f(v,u[k])   end; return v end
function kaps(t,u,f,  v) v={}; for k,v in pairs(t) do v[1+#v]=f(k,v,u[k]) end; return v end

function cat(t,    key,u)
  function key(k,v) if (tostring(k)):sub(1,1)~="_" then return fmt(":%s %s",k,v) end end
  u=  #t>1 and  map(t,f or tostring) or sort(kap(t,key))
  return (t._is or "").."{"..table.concat(u," ").."}" end

function chat(t) print(cat(t)); return t end

function thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false else
    return math.tointeger(x) or tonumber(x) or x end  end

function words(s,sep,fun,      t)
   fun = fun or same
   t={};for x in s:gmatch(fmt("([^%s]+)",sep)) do t[1+#t]=fun(x) end; return t end

function csv(file, fun)
  local file = io.input(file)
  while true do
    local line = io.read()
    if not line then return io.close(file) else fun(words(line, ",",thing)) end end end

local _id = 0
function obj(name,fun,    t,new,x)
  function new(kl,...) _id=_id+1; x=setmetatable({_id=_id},kl);fun(x,...); return x end 
  t = {__tostring=cat,_is=name}; t.__index=t
  return setmetatable(t, {__call=new}) end
--------------------------------------------------------------------------------
local function _col(i,at,txt) 
  i.n,i.at,i.txt,i.kept = 0,at or 0 ,txt or "",{}
  i.w = i.txt:find"-$" and -1 or 1  end

local Sym=obj("Sym", _col)
local Num=obj("Num", function(i,at,txt) _col(i,at,txt); i.nums,i.ok = 256,true end)

function col(at,txt) return (name:find"^[A-Z]" and Num or Sym)(at,txt) end

local Cols=obj("Cols",function(i,names) 
  i.x, i.y, i.klass, i.names = {}, {}, nil, names 
  i.all = kap(names,col)
  for _,col in pairs(i.all) do
    if not col.txt:find":$" then
      push(col.txt:find"[!+-]$" and i.y or i.x, col)
      if col.txt:find"!$" then i.klass=col end end end end)

function Cols.add(i,row)
  for _,cols in pairs{i.x,i.y} do
    for _,col in pairs(cols) do col:add(row.cells[col.at]) end end end
--------------------------------------------------------------------------------
local go,no={},{}

function go.CHAT() chat{aa=1,bb=3,cc={1,2,3}}; return true end

function go.ALL() 
  local fails,old = 0,{} 
  for k,v in pairs(the) do old[k]=v end
  for k,v in pairs(go) do
    if k~="ALL" then
      math.randomseed(the.seed or 10019)
      if v() ~= true then print("FAIL",k); fails=fails+1 end  
      for k,v in pairs(old) do the[k]=v end end end
  os.exit(fails) end


(go[arg[2]] or same)()  

-- local Rows=obj("Row", function(i,row) i.rows={}; i.cols=nil; i.categories={} end)
-- function Rows.add(i,row)
--   rs.kepts = rs.cols and maps(r.kepts,row,update) or i:categorize(kap(row,init) end)
--
-- function Rows.categorize(i,cols)
--   for _,col in pairs(cols) do if not col.ignorep then 
--      push(col.txt:find"[!+-]$" and i.categories.y or i.categories.y, col) end end 
--   return end
--
-- function make(f,rows) 
--   local function make1(row) if rows then rows:add(row) else rows=Rows(row) end
--   if type(src)=="table" then map(rows,make1) else csv(src,make1) end
--   return rows end
