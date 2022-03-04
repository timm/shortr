local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local the, help = {}, [[
OPTIONS:
  -file  -f file          = ../../etc/data/auth93.csv
  -keep  -k items to keep = 256
  -cohen -c cohen         = .35
  -tiny  -t tiny          = .5
]]

local function thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return tonumber(x) or x end

local function file2things(file,      x)
  local function things(x,  t)
    t={}; for y in x:gmatch("([^,]+)") do t[1+#t]=thing(y) end; return t end
  file = io.input(file)
  return function()
    x=io.read(); if x then return things(x) else io.close(file) end end end

local as = setmetatable
local function obj(   t)
  t={__tostring=o}; t.__index=t
  return as(t, {__call=function(_,...) return t.new(_,...) end}) end

local Num, Sym = obj(), obj()
local function Sym:new(at,txt) 
    return as({at=at, txt=txt, n=0,all={}},Sym) end

local function Num:new(at,txt) 
    return as({at=at, txt=txt, all={}, ok=false, n=0,sd=0,mu=0,m2=0,
               lo=math.huge, hi=-math.huge},Num) end

function Sym.add(i,x,inc)
  if x ~= "?" then
    inc = inc or 1
    i.n = i.n+inc
    i.all[x] = inc + (i.all[x] or 0)
    if i.all[x] > i.most then i.most, i.mode = i.all[x], x end end end

function Sym.sub(i,x,inc)
  if x ~= "?" then
    inc = inc or 1
    i.n = i.n - inc
    i.all[x] = i.all[x] - inc end end

function Sym.div(i,  e)
  e=0; for _,n in pairs(i.all) do e=e + n/i.n*math.log(n/i.n,2) end
  return -e end

function Num.div(i) return i.sd end
function Num.add(i,x,_,    d)
  if x ~="?" then
    i.n   = i.n + 1
    d     = x - i.mu
    i.mu  = i.mu + d/i.n
    i.m2  = i.m2 + d*(x - i.mu)
    i.sd  = (i.m2<0 or i.n<2) and 0 or ((i.m2/(i.n-1))^0.5)
    i.lo  = math.min(x, i.lo)
    i.hi  = math.max(x, i.hi) 
    if     #i.all < the.keep  then i.ok=false; push(i.all,x)  
    elseif r() < the.keep/i.n then i.ok=false; i.all[r(#i.all)]=x end end end

function Num.sub(i,x,_,    d)
  if x ~="?" then
    i.n   = i.n - 1
    d     = x - i.mu
    i.mu  = i.mu - d/i.n
    i.m2  = i.m2 - d*(x - i.mu)
    i.sd  = (i.m2<0 or i.n<2) and 0 or ((i.m2/(i.n-1))^0.5) end end

function Num.spans(i,j)
  local xys,all = {}, Num
  for _,n in pairs(i.all) do all:add(n); push(xys,{x=n,y="left"}) end
  for _,n in pairs(j.all) do all:add(n); push(xys,{x=n,y="right"})  end
  return bins(i,sort(xys,first),(#xys)^the.minItems,all.sd*the.cohen,Sym,cuts) end
  
function xpect(i,j) 
  return (i.n*i:div() + j.n*j.div()) / (i.n + j,n) end

--- get rid of sub call
local function bins(col,xys,minItems,cohen,yclass,cuts)
  local cuts,xpect,split,splits = {}
  function splits(b4,xys,       cut,lhs,rhs,div,l,r)
    lhs,rhs = yclass(), yclass()
    for _,xy in pairs(xys) do rhs:add(xy.y) end
    div = rhs:div()
    for j,xy in pairs(xys) do
      lhs:add(xy.y)
      rhs:sub(xy.y)
      if lhs.n >= minItems and rhs.n >= minItems then
        if xy.x ~= xys[j+1].x then
          if xy.x - xys[1].x >= cohen and xys[#xys].x - xy.x >= cohen then
            if xpect(lhs,rhs) < div then 
              cut, div = j, xpect(lhs,rhs) end end end end end
    if   cut 
    then l,r = {},{}
         for n,xy in pairs(xys) do push(n<=cut and l or r, xy) end
         splits(b4,         l)
         splits(xys[cut].x, r)
    else push(cuts, {col=col,lo=b4, hi=xys[#xys].x, n=#xys, div=div}) end 
    return cuts
  end ------------------
  return splits(xys[1].x, xys)  end
  
local function Egs(row, i)
  i = i or {rows={}, x={}, y={}, nums={}, syms={}, cols={}}
  local function header(col, at, name,    now,here)
    now = (name:find"^[A-Z]" and Num or Sym)(at,name)
    i.cols[1 + #i.cols] = now
    if not name:find":$" then
      here = name:find"[+-!]$" and i.y    or i.x   ; here[1 + #here] = now
      here = name:find"^[A-Z]" and i.nums or i.syms; here[1 + #here] = now
      if name:find"!$" then i.class = now end end 
  end -------------------------------------------
  if   #i.cols==0 
  then i.rows[1 + #i.rows] = row
       for at,col  in pairs(i.cols) do col:add(row[col.at])  end
  else for at,name in pairs(row)    do header(col, at, name) end end
  return i end

--------------------------------
help:gsub("\n  ([-]([^%s]+))[%s]+(-[^%s]+)[^\n]*%s([^%s]+)",
  function(long,key,short,x)
    for n,flag in ipairs(arg) do if flag==short or flag==long then
      x = x=="false" and true or x=="true" and "false" or arg[n+1] end end 
    if x=="false" then the[key]=false elseif x=="true" then the[key]=true else
  the[key] = thing(x) end end)

names=nil
for row in file2thing(the.file) do
  if not names then names = row else
    
