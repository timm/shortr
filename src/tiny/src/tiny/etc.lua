local l={}
-- Cache names -----------------------------------------------------------------
local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end
function l.rogues()
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end end

-- Print table ----------------------------------------------------------------
function l.cat(t,        show,u,pub)
  if type(t)~="table" then return tostring(t) end
  function show(k,v)
    if not tostring(k):find"^[A-Z]"  then
      v=l.cat(v)
      return #t==0 and string.format(":%s %s",k,v) or tostring(v) end end
  u={}; for k,v in pairs(t) do u[1+#u] = show(k,v) end
  table.sort(u)
  return (t._is or "").."{"..table.concat(u," ").."}" end

-- Update slots in `t` from command line ---------------------------------------
function l.cli(t)
  for slot,v in pairs(t) do
    v = tostring(v)
    for n,x in ipairs(arg) do
      if x=="-"..(slot:sub(1,1)) or x=="--"..slot then
        v = v=="false" and "true" or v=="true" and "false" or arg[n+1] end end
    t[slot] =  l.coerce(v) end
  return t end

-- Define classes --------------------------------------------------------------
function l.klass(name,     new,self,t)
  function l.new(k,...)
    self = setmetatable({},k)
    return setmetatable(k.new(self,...) or self,k) end
  t={_is = name, __tostring = l.cat}
  t.__index = t
  return setmetatable(t,{__call=new}) end

-- Coerce ----------------------------------------------------------------------
function l.coerce(str)
  local function coerce1(str)
    if str=="true"  then return true end 
    if str=="false" then return false end
    return str end 
  return tonumber(str) or coerce1(str:match"^%s*(.-)%s*$") end

-- Coerce lines from csv file (fiterling result through `fun`).
function l.csv(filename, fun)
  l.lines(filename, function(t) fun(l.words(t,",",l.coerce)) end) end

--- Call `fun  on all lines from `filename`.
function l.lines(filename, fun)
  local src = io.input(filename)
  while true do
    local str = io.read()
    if not str then return io.close(src) else fun(str) end end end

-- Split  `str` on `sep`, filtering parts through `fun`.
function l.words(str,sep,fun,      t)
  fun = fun or function(z) return z end
  sep = l.string.format("([^%s]+)",sep)
  t={};for x in str:gmatch(sep) do t[1+#t]=fun(x) end;return t end

return l
