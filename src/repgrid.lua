local the,help = {},[[
tweak: tries three weak learners for multi-objective optimization
(c) 2022, Tim Menzies,  timm@ieee.org, opensource.org/licenses/Fair

USAGE:
  alias twk="lua tweak.lua "
  twk [OPTIONS]

OPTIONS:
  --boot    -b  size of bootstrap           = 512
  --cohen   -c  cohen                       = .35

OPTIONS (other):
  --file    -f  where to find data          = ../etc/data/auto2.csv
  --dump    -d  dump stack+exit on error    = false
  --help    -h  show help                   = false
  --go      -g  start up action             = nothing]] 

local rows, aotm = {}
R=math.random

function atom(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x end

help:gsub("\n  ([-][-]([^%s]+))[%s]+(-[^%s]+)[^\n]*%s([^%s]+)",function(f1,k,f2,x)
  for n,flag in ipairs(arg) do if flag==f1 or flag==f2 then
    x = x=="false" and"true" or x=="true" and"false" or arg[n+1] end end 
  the[k] = atom(x) end) 

function csv(f)
  f = io.input(f)
  return function(t, u)
    t=io.read()
    if not t then io.close(f) else
      u={}; for x in t:gmatch("([^,]+)") do u[1+#u]=atom(x) end
      return u end end end

function map(t,f, u)  u={}; for _,v in pairs(t) do u[1+#u]=f(v) end;return u end
function push(t,x)    t[1+#t]=x; return x end
f
function o(t,    u)
  u={}; for k,v in pairs(t) do u[1+#u] = string.format(":%s %s",k,v) end
  return (t.is or "").."{"..table.concat(sort(u)," ").."}" end

function obj(name,    t,new)
  function new(kl,...) 
    local x=setmetatable({id=id()},kl); kl.new(x,...); return x end 
  t = {__tostring=o, is=name or ""}; t.__index=t
  return setmetatable(t, {__call=new}) end

--------------------------------------------------------------------------------
local NUM=obj"NUM"
function NUM:new(i,txt) 
  self.i,self.txt, self.lo,self.hi,self.bins = i,txt,math.huge,-math.huge,{} end

function NUM:addx(x)
  if x~="?" then
    self.lo = math.min(x, self.lo)
    self.hi = math.max(x, self.hi) end end

function NUM:norm(x)
  return x=="?" and x or (x-self.lo)/(self.hi - self.lo) end

function NUM:addxy(x,y)
  if x=="?" then return x end
  x = math.max(1, math.min(16, 16*self:norm(x) // 1))
  --- bins recurisive
  self.bins[x] = y + (self.bins[x] or 0) end

local SYM=obj"SYM"
function SYM:new(i,txt) 
  self.i, self.txt, self.has,self.bins = i,txt,{},{} end

function SYM:addx(x) 
  if x~="?" then self.has[x] = 1+(self.has[x] or 0) end end

function SYM:addxy(x,y) 
  if x~="?" then self.bins[x] = y + (self.bins[x] or 0) end end

function SYM:mid(   m,x)
  m=0; for y,n in pairs(self.has) do if n>m then m,x=y,n end end; return m end

function SYM:div(   n,e)
  n=0; for _,m in pairs(self.has) do n = n + m end 
  e=0; for _,m in pairs(self.has) do e = e - m/n*math.log(m/n,2) end 
  return e end

function ROW:new(egs,t) 
  self.cells,self.data = t,egs end

local COLS=obj"COLS"
function COLS:new(names,     col)
  self.all,self.x,self.y,self.names={},{},{},names
  for i,txt in pairs(names) do
    col = push(self.all, txt.find"^[A-Z]+" and Num or Sym)(i,txt))
    if not txt:find":$" then
      push(txt.find"[-+!]$" and self.y or self.x,col) end end end 

local EGS=obj"EGS"
function EGS:new() self.rows,self.cols= {},nil end
function EGS:add(t)
  if   self.cols 
  then t = push(self.rows, t.cells and t or ROW(self,t)).cells
       for _,col in pairs(self.cols.all) do col:add(t[col.pos]) end
  else self.cols = COLS(t) end end

function EGS:file(f) for row in csv(f) do self.add(row) end end
--------------------------------------------------------------------------------

