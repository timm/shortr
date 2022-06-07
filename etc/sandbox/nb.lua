local help= [[
NB:  
(c)2022 Tim Menzies, timm@ieee.org

OPTIONS:
  --k     -k  handle rare classes    = 1  
  --m     -m  handle rare attributes = 2
  --p     -p  distance coefficient   = 2

OPTIONS (other):
  --help  -h  show help     = false
  --go    -g  start-up goal = nothing
  --seed  -s  seed          = 10019
  --file  -f  file          = ../../data/auto93.csv]]
--------------------------------------------------------------------------------
local lib = require"lib"
local cli,csv,demo,normpdf = ilib.cli, lib.csc, lib.demo, lib.normpdf
local o,read,str      = lib.o, lib.read, lib.str

local THE={}
help:gsub(" [-][-]([^%s]+)[^\n]*%s([^%s]+)",function(key,x) THE[key]=read(x) end)
--------------------------------------------------------------------------------
local function sym(f,col,kl,x,        d,i,j)
  j = f[col]; if j==nil then j={}; f[col] = j end
  i = j[kl];  if i==nil then i={n=0,mu=0,m2=0,sd=0}; j[kl] = i end
  i.n  = i.n+1
  d    = x-i.mu
  i.mu = i.mu + d/i.n
  i.m2 = i.m2 + d*(x - i.mu)
  i.sd = i.n<2 and 0 or (i.m2/(i.n-1))^0.5 end 

local function num(f,col,kl,x,        i,j)
  j = f[col]; if j==nil then j={}; f[col] = j end
  i = j[kl];  if i==nil then i={has={},most=0,mode=nil}; j[kl] = i end
  i.n = i.n + 1
  i.has[x] = (inc or 1) + (i.has[x] or 0)
  if i.has[x] > i.most then i.most,i.mode = i.has[x],x end end 
--------------------------------------------------------------------------------
local function nb(   kl)
  local i={fun={}, n=-1, f={}}
  for row in csv(THE.file) do
     i.n = i.n + 1
     kl = row[#row]
     for col,x in pairs(row) do 
       if   i.n==0 
       then if k<#row then i.fun[col] = x:find"^[A-Z]" and num or sym end 
       else if x~="?" then i.fun[col](i.f,col,kl,x) end end end end end
--------------------------------------------------------------------------------
local no,go = {},{}
function go.csv() for row in csv(THE.file)  do oo(row) end end
--------------------------------------------------------------------------------
if    pcall(debug.getlocal, 4, 1)
then  return {ROW=ROW, ROWS=ROWS, NUM=NUM, SYM=SYM, THE=THE,lib=lib} 
else  THE = cli(THE,help)
      demos(THE,go) end
