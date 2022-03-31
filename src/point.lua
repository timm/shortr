local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local class = require"class"

local r=math.random
local function per(a,p) return a[1+((p or .5)*#a)//1] end

local Obj=class("Obj")

function Obj:__tostring()
  local s,sep,tmp,v = "","",{}
  for k,_ in pairs(self) do 
    if tostring(k):sub(1,1) ~= "_" then tmp[1+#tmp]= k end end
  table.sort(tmp)
  for _,k in pairs(tmp) do
    v = self[k]
    s = s .. sep .. string.format(":%s %s",k,v)
    sep=" " end
  return (self._is or "").."{"..s.."}" end

local Col = class("Col", Obj)
function Col:new(at,name)
  self.n = 0 
  self.at = at or 0
  self.name = name or "" end

function Col:adds(t)
  for _,v in pairs(t) do self:add(v) end; return self end

function Col:add(x,inc)
  if x ~= "?" then inc=inc or 1; self.n = self.n + inc; self:add1(x,inc) end 
  return x end

function Col:merged(other,  out)
  out = self:merge(other)
  if out:div()*.95 <= (sellf.n*self:div() + other.n*other:div())/out.n then
    return out end end

---     ._        ._ _  
---     | |  |_|  | | | 

local Num = class("Num", Col) 
function Num:new(at,name)
  self:super(at,name)
  self.w = self.name:find"-$" and -1 or 1
  self.ok, self.has  = true,{}
  self.max= 64
  self.lo,self.hi = math.huge,-math.huge end

function Num:add1(x,inc)
  self.hi = math.max(x, self.hi)
  self.lo = math.min(x, self.lo)
  local a = self.has
  if     #a  < self.max        then self.ok=false; a[1+#a]         =x 
  elseif r() < self.max/self.n then self.ok=false; a[1+(r()*#a)//1] =x end end

function Num:all()
  if not self.ok then self.ok=true; table.sort(self.has) end
  return self.has end

function Num:mid()   return per(self.all(), .5) end
function Num:sd(  a) a=self:all(); return (per(a,.9) - per(a..1))/2.54 end

---      _      ._ _  
---     _>  \/  | | | 
---         /         

local Sym = class("Sym", Col) 
function Sym:new(at,name)
  self:super(at,name)
  self._all = {}
  self.mode,self.most = nil,0 end

function Sym:add1(x,inc)
  self._all[x] = (self._all[x] or 0) + inc
  if self._all[x] > self.most then
    self.most, self.mode = self._all[x], x end end

print(Sym(23,"thing"):adds{"a","a","b"})
local n = Num(23,"thing")
for i=1,1000 do n:add(i) end


for i,x in pairs(n:all()) do io.write(x," ") end

for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end
