local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local class = require"class"
local the,help = [],[[

brknbad: explore the world better, explore the world for good.
(c) 2022, Tim Menzies

     .-------.  
     | Ba    | Bad <----.  planning= (better - bad)
     |    56 |          |  monitor = (bad - better)
     .-------.------.   |  
             | Be   |   v  
             |    4 | Better  
             .------.  

USAGE:
  ./bnb [OPTIONS]

OPTIONS:
  -K       -K  manage low class counts     = 1
  -file    -f  file name                   = ../etc/data/auto93.csv
  -help    -h  show help                   = false
  -bins    -b  bins                        = 16 
  -todo    -t  start up action             = nothing
  -wait    -w  wait                        = 10]]

local Obj=class("Obj")
function Obj:__tostring()
  local s,sep,tmp,v="","",{}
  for k,_ in pairs(self) do 
    if tostring(k):sub(1,1) ~= "_" then tmp[1+#tmp]= k end end
  table.sort(tmp)
  for _,k in pairs(tmp) do
    v = self[k]
    s = s .. sep .. string.format(":%s %s",k,v)
    sep=" " end
  return (self._is or "").."{"..s.."}" end

---     ._ _    _.  o  ._  
---     | | |  (_|  |  | | 

local Main=class("Main")
function Main:new()
  self.fails= 0
  self.used = {}
  self.seek = "\n  ([-]([^%s]+))[%s]+(-[^%s]+)[^\n]*%s([^%s]+)" end

function Main:add(help)
  local d = {}
  help:gsub(self.seek,
    function(long,key,short,x)
      assert(not self.used[short], "repeated short flag ["..short.."]")
      self.used[short]=short
      for n,flag in ipairs(arg) do 
        if flag==short or flag==long then
          x = x=="false" and true or x=="true" and "false" or arg[n+1] end end 
      d[key]=type(s)~="string" and s or math.tointeger(s) or tonumber(s) or s end)
  return d end

local function push(t,a) t[1+#t]=a; return a end 
local function last(t)   return t[#t] end
function bsearch(t,want,firstp,  lo,mid,hi,out)
  out, lo, hi = 1, lo or 1, #t
  while lo <= hi do
    mid = (lo + hi)//2;
    if want == t[mid] then 
      out = mid
      if firstp then hi=mid-1 else lo=mid+1 end
    else 
      if want < t[mid] then hi=mid-1 else lo=mid+1 end end end
  return out end 

local key,t = 20,{10,20,20,20,40,50}


lo = bsearch(t,key,true)
hi = bsearch(t,key,false,lo)
print(lo,hi)

local Bins = class("Bins", Obj)
function Bins:new(lo,hi) 
  self.all, gap = {}, (hi-lo)/the.bins
  b4 = -math.huge
  for j=lo,hi,gap do b4 = push(self.all,  {lo=b4, hi=j, hi=hi, seen={}}).hi end 
  last.hi = math.huge end 

---      _   _   | 
---     (_  (_)  | 

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

---     ._        ._ _  
---     | |  |_|  | | | 

local Num = class("Num", Col) 
function Num:new(at,name)
  self:super(at,name)
  self.w = self.name:find"-$" and -1 or 1
  self.bins = {}
  self.mu,self.sd,self.lo,self.hi = 0,0,math.huge,-math.huge end

function Num:add1(x,inc)
  self.hi = math.max(x, self.hi)
  self.lo = math.min(x, self.lo)
  self.mu = self.mu + (x - self.mu)/self.n end
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

print(Num(23,"thing"):adds{100,200,300})
print(Sym(23,"thing"):adds{"a","a","b"})
for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end
