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

local function isa(t,meta,inits) 
  setmetatable(meta, getmetatable(t))
  setmetatable(t,meta); for k,v in pairs(inits or {}) do t[k]=v end; return t end

local _id = 0
local function new(kl,...) return kl.new(isa({},kl),...) end 

local function obj(name,base,    t,mt)
  t = {__tostring=cat,_is=name}; t.__index=t
  t= isa(t, {__call=new}) 
  return t end

local OBJ=obj"OBJ"  
function OBJ:new() _id=_id+1; self.id=_id; return self end
function OBJ:add(t) print(1);for k,v in pairs(t or {}) do self[k]=v end; return self end
  
local COL=obj("COL",OBJ)  
function COL:new(at,txt) return isa(OBJ(), COL,{at=at or 0,txt=txt or "",n=0, kept={}}) end

function COL:add(x,n)
  n= n or 1
  print(cat(self))
  if x ~="?" then self.n=self.n+n self:add1(x,n) end end 

local SYM=obj"SYM"
function SYM:new(...)
  self=isa(COL(...),SYM)
  self.mode, self.most = nil,0 
  return self
end

function SYM:add1(x,n) 
  self.kept[x] = n + (self.kept[x] or 0) end

s=SYM(2,"tim")
s:add("t")
s:add("t")
s:add("t")
print(s)
