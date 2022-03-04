local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local the, help = {}, [[
OPTIONS:
  -file -f file ../../etc/data/auth93.csv
]]

local function thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return tonumber(x) or x end

local function things(x,sep,  t)
  t={}; for y in x:gmatch(sep or"([^,]+)") do t[1+#t]=thing(y) end
  return t end

local function file2things(file,      x)
  file = io.input(file)
  return function()
    x=io.read(); if x then return things(x) else io.close(file) end end end

local function Egs(row, i)
  i = i or {rows={}, x={}, y={}, nums={}, syms={}, cols={}}
  local function Sym(at,txt) 
    return {at=at, txt=txt, all={}} end
  local function Num(at,txt) 
    return {at=at, txt=txt, all={}, ok=false, lo=math.huge, hi=-math.huge} end
  local function datum(col, at, cell)
    if cell ~= "?" then  
      if   i.nums[at] 
      then col.all[1+#col.all]= cell 
           col.ok = false
           col.lo = math.min(cell, col.lo) 
           col.hi = math.max(cell, col.hi)
      else col.all[cell] = 1 + (col.all[cell] or 0) end end 
  end -----------------------------------------
  local function header(col, at, name,    now,here)
    now = (name:find"^[A-Z]" and Num or Sym)(at,name)
    i.cols[1 + #i.cols] = now
    if not name:find":$" then
      here = name:find"[+-!]$" and i.y    or i.x   ; here[1 + #here] = now
      here = name:find"^[A-Z]" and i.nums or i.syms; here[1 + #here] = now
      if name:find"!$" then i.class = now end end 
  end ----------
  if   #i.cols==0 
  then i.rows[1 + #i.rows] = row
       for at,col  in pairs(i.cols) do datum(col,  at, row[col.at]) end
  else for at,name in pairs(row)    do header(col, at, name) end end
  return i end

help:gsub("\n  ([-]([^%s]+))[%s]+(-[^%s]+)[^\n]*%s([^%s]+)",
  function(long,key,short,x)
    for n,flag in ipairs(arg) do if flag==short or flag==long then
      x = x=="false" and true or x=="true" and "false" or arg[n+1] end end 
    if x=="false" then the[key]=false elseif x=="true" then the[key]=true else
  the[key] = tonumber(x) or x end end)

names=nil
for row in file2thing(the.file) do
  if not names then names = row else
    
