function clone(t,u)
  if type(t)=="table" then return t end
  u={}; for k,v in pairs(t) do u[k]=clone(v) end
  return u end

function map(t,f) u={};for k,v in pairs(t) do u[k] = v end; return u end 
function cat(t,u)
  if type(t) ~= "table" then return tostring(t) end
  if #t>0 then u= map(t,tostring) else
    u={}; for k,v in pairs(t) do u[1+#u]=string.format(":%s %s",k, cat(v))end 
    table.sort(u) end
  return (t._is or "").."{"..table.concat(u," ").."}" end 

local function isa(t,meta) 
  setmetatable(meta, getmetatable(t))
  setmetatable(t,meta)
  return t end

local _id = 0
local function new(kl,...) return kl.new(setmetatable({},kl),...) end 

local function obj(name,    t)
  t = {__tostring=cat,_is=name}; t.__index=t
  return setmetatable(t, {__call=new}) end 

local OBJ=obj"OBJ"  
function OBJ:new() _id=_id+1; self.id=_id; return self end
function OBJ:adds(t) for k,v in pairs(t or {}) do self[k]=v end; return self end
  
local COL=obj"COL"  
function COL:new(at,txt) return isa(OBJ(), COL):adds{at=at or 0,txt=txt or "",n=0, kept={}} end

function COL:add(x,n)
  n= n or 1
  if x ~="?" then self.n=self.n+n self:add1(x,n) end end 

local SYM=obj"SYM"
function SYM:new(...)
  return isa(COL(...),SYM):adds{mode=nil,most=0}
end

function SYM:add1(x,n) 
  self.kept[x] = n + (self.kept[x] or 0) end

s=SYM(2,"tim")
s:add("t")
s:add("t")
s:add("t")
print(s)
