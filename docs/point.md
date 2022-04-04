<a name=top>&nbsp;<br>
<img align=left width=175 src="/docs/head.png"> <img 
src="https://img.shields.io/badge/purpose-se,ai-informational?style=flat&logo=hyper&logoColor=white&color=red"> <img 
src="https://img.shields.io/badge/language-lua-informational?style=flat&logo=lua&logoColor=white&color=yellow"> <img 
src="https://img.shields.io/badge/platform-osx,linux-informational?style=flat&logo=linux&logoColor=white&color=orange"> <a
href="https://github.com/timm/l5/actions/workflows/tests.yml"><img src="https://github.com/timm/l5/actions/workflows/tests.yml/badge.svg"></a> <a 
href="https://zenodo.org/badge/latestdoi/206205826"> <img src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a><br>
<b> <a href="https://github.com/timm/l5/blob/master/LICENSE.md">&copy;2022</a> </b>, Tim Menzies, <timm@ieee.org>
<h1>The Little LUA Learning Library</h1><br clear=all>



```lua
local r = math.random
local fmt=string.format
local function per(a,p) return a[1+((p or .5)*#a)//1] end
local function sort(t,f) table.sort(t,f); return t end
```



```lua
local Obj=class("Obj")
```



```lua
function Obj:show(  t)
  t={}
  for k,v in pairs(self) do if tostring(k):sub(1,1)~="_" then t[1+#t]=k end end
  return sort(t) end
```



```lua
function Obj:__tostring(  u)
  u={}; for _,k in pairs(self:show()) do u[1+#u] = fmt(":%s %s",k,self[k]) end
  return self._is .."{"..table.concat(u," ").."}" end
```



```lua
local Col = class("Col", Obj)
function Col:new(at,name)
  self.n = 0 
  self.at = at or 0
  self.name = name or "" end
```



```lua
function Col:adds(t)
  for _,v in pairs(t) do self:add(v) end; return self end
```



```lua
function Col:add(x,inc)
  if x ~= "?" then inc=inc or 1; self.n = self.n + inc; self:add1(x,inc) end 
  return x end
```



```lua
function Col:merged(other,  out)
  out = self:merge(other)
  if out:div()*.95 <= (sellf.n*self:div() + other.n*other:div())/out.n then
    return out end end
```



```lua

local Num = class("Num", Col) 
function Num:new(at,name)
  self:super(at,name)
  self.w = self.name:find"-$" and -1 or 1
  self.ok, self.has  = true,{}
  self.max= 64
  self.lo,self.hi = math.huge,-math.huge end
```



```lua
function Num:add1(x,inc)
  self.hi = math.max(x, self.hi)
  self.lo = math.min(x, self.lo)
  local a = self.has
  if     #a  < self.max        then self.ok=false; a[1+#a]         =x 
  elseif r() < self.max/self.n then self.ok=false; a[1+(r()*#a)//1] =x end end
```



```lua
function Num:all()
  if not self.ok then self.ok=true; table.sort(self.has) end
  return self.has end
```



```lua
function Num:mid()   return per(self:all(), .5) end
function Num:div(  a) a=self:has(); return (per(a,.9) - per(a..1))/2.54 end
```



```lua
function Num:same(x,y) return math.abs(x-y) <= self:div()*.35 end
```



```lua
function Num:merge(other,   out)
  out = Num(self.at, self.name)
  for _,n in pairs(self.has)  do out:add(n) end
  for _,n in pairs(other.has) do out:add(n) end
  return out end
```



```lua

local Sym = class("Sym", Col) 
function Sym:new(at,name)
  self:super(at,name)
  self.has = {}
  self.mode,self.most = nil,0 end
```



```lua
function Sym:add1(x,inc)
  self.has[x] = (self.has[x] or 0) + inc
  if self.has[x] > self.most then
    self.most, self.mode = self.has[x], x end end
```



```lua
function Sym:mid() return self.mode end
function Sym:div(  e,p) 
  e=0; for _,v in pairs(self.has) do p=v/self.n; e = e - p*math.log(p,2) end
  return e end
```



```lua
function Sym:merge(other,   out)
  out = Sym(self.at, self.name)
  for x,n in pairs(self.has)  do out:add(x,n) end
  for x,n in pairs(other.has) do out:add(x,n) end
  return out end
```



```lua
print(Sym(23,"thing"):adds{"a","a","b"})
local n = Num(23,"thing")
for i=1,1000 do n:add(i) end
```



```lua
for i,x in pairs(n:all()) do io.write(x," ") end
```



```lua
for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end
