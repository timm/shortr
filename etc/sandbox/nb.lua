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
local cli,csv,demo,normpdf = ilib.cli, lib.csc, lib.demo, ib.normpdf
local o,read,str      = lib.o, lib.read, lib.str

local THE={}
help:gsub(" [-][-]([^%s]+)[^\n]*%s([^%s]+)",function(key,x) THE[key]=read(x) end)

--------------------------------------------------------------------------------
local function load(src, fun)
  if type(src)~="string" then for _,t in pairs(src) do fun(t) end
                         else for   t in csv(src)   do fun(t) end end end

--------------------------------------------------------------------------------
local function usep(x)   return not x:find":$" end
local function nump(x)   return x:find"^[A-Z]" end
local function goalp(x)  return x:find"[!+-]$" end
local function klassp(x) return x:find"!$"     end

local function NUM() return {n=0,mu=0, m2=0, sd=0, nump=true} end
local function SYM() return {n=0,syms={}, most=0, mode=nil} end

local function COL(at,txt)
  local i = (nump(i.txt) and NUM or SYM)()
  i.at, i.txt = at or 0, txt or ""
  i.nump, i.w = nump(i.txt), i.txt:find"-$" and -1 or 1
  return i end

local function COLS(t)
  local i={all={}, xs={}, ys={}, names=t}
  for at,x in pairs(t) do
    col = push(i.all, COL(at,x))
    if col.usep  then 
      if klassp(col.txt) then i.klass=col end
      push(goalp(col.txt) and i.ys or i.xs, col) end end
  return i end

local function col1(i, x)
  local function num(v)
    d    = v - i.mu
    i.mu = i.mu + d/i.n
    i.m2 = i.m2 + d*(v - i.mu)
    i.sd = i.n<2 and 0 or (i.m2/(i.n-1))^0.5 end 
  local function sym(v)
    i.syms[v] = (inc or 1) + (i.syms[v] or 0)
    if i.syms[x] > i.most then i.most,i.mode = i.syms[v],v end end
  for _,v in pairs(type(x)=="table" and x or {x}) do
    if v ~="?" and i.usep then 
      i.n = i.n + 1
      (i.nump and num or sym)(v) end end
  return x end

local function ROWS(t,i)
  i = i or {cols=nil,  rows={}}
  if    i.cols 
  then  push(i.rows, t)
        for _,col in pairs(i.cols) do add(col, t[col.at]) end 
  else  i.cols = COLS(t) end
  return i end

local function clone(i,init) return row(init or {},  row({i.names})) end

local function like(i,t, nklasses, nrows,    prior,like,inc,has)
  prior = (i.n + THE.k) / (nrows + THE.k * nklasses)
  like  = math.log(prior)
  for c,x in pairs(t) do
    if x ~= "?" and c~=#t then
      has = i.cols[c].has
      if   i.isa[c] == num 
      then inc= normpdf(x, has.mu, has.sd)
      else inc= ((has.seen[x] or 0) + THE.m*prior) / (has.n + THE.m) end
      like = like + math.log(inc) end end
  return like end

function nb(src,i)
  i=i or {all=
--------------------------------------------------------------------------------
local no,go = {},{}
function go.csv() for row in csv(THE.file)  do oo(row) end end
--------------------------------------------------------------------------------
if    pcall(debug.getlocal, 4, 1)
then  return {ROW=ROW, ROWS=ROWS, NUM=NUM, SYM=SYM, THE=THE,lib=lib} 
else  THE = cli(THE,help)
      demos(THE,go) end
