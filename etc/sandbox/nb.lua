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

local big,cli,copy,csv,demos  = lib.big, lib.cli, lib.copy,lib.csv,lib.demos
local fmt,is,map,normpdf,oo   = lib.fmt,  lib.is,lib.map, lib.normpdf,lib.oo
local normpdf,pop,push,rand   = lib.normpdf, lib.pop,lib.push,lib.rand
local read,rnd,shuffle,splice = lib.read, lib.rnd, lib.shuffle, lib.splice
local str                     = lib.str

local THE={}
help:gsub(" [-][-]([^%s]+)[^\n]*%s([^%s]+)",function(key,x) THE[key]=read(x) end)

local COLS,NB,ROW,ROWS,NUM,SYM = is"COLS",is"NB",is"ROW",is"ROWS",is"NUM",is"SYM"
--------------------------------------------------------------------------------
local function load(src,i)
  if type(src)=="string" then for   row in csv(  src) do i:add(row) end 
                         else for _,row in pairs(src) do i:add(row) end end 
  return i end
--------------------------------------------------------------------------------
function SYM.new(i, at,txt) 
  i.n, i.at, i.txt = 0, at or 0, txt or ""
  i.has, i.most, i.mode = {}, 0, nil end

function SYM.add(i,x,inc)
  if x=="?" then return x end
  i.n = i.n + 1
  i.has[x] = (inc or 1) + (i.has[x] or 0)
  if i.has[x] > i.most then i.most,i.mode = i.has[x],x end end 

function SYM.like(i,x,prior) 
  return ((i.has[x] or 0) + THE.m*prior) / (i.n + THE.m) end
--------------------------------------------------------------------------------
function NUM.new(i, at,txt) 
  i.n, i.at, i.txt = 0, at or 0, txt or ""
  i.w = i.txt:find"-$" and -1 or 1  
  i.mu, i.m2, i.sd, i.lo, i.hi = 0, 0, 0, big, -big end

function NUM.add(i,x,     d)
  if x=="?" then return x end
  i.n  = i.n+1
  d    = x-i.mu
  i.mu = i.mu + d/i.n
  i.m2 = i.m2 + d*(x - i.mu)
  i.sd = i.n<2 and 0 or (i.m2/(i.n-1))^0.5 
  i.lo = math.min(x,i.lo)
  i.hi = math.max(x,i.hi) end

function NUM.like(i,x,...) return  normpdf(x, i.mu, i.sd) end
--------------------------------------------------------------------------------
function ROW.new(i,of,cells) i.of, i.cells = of, cells end

function ROW.like(i,nklasses,all,       prior,like,x)
  prior = (#i.has + THE.k) / (all + THE.k * nklasses)
  like  = math.log(prior)
  for _,col in pairs(i.of.cols.xs) do
    x = i.cells[col.at]
    if x and x ~= "?" then like = like + math.log(col:like(x,prior)) end end
  return like end

function ROW.klass(i) return i.cells[i.of.cols.klass.at] end

function ROW.guess(i,klasses,     liked,most,tmp,n)
  n, most, liked = 0, -big, nil
  for _,klass in pairs(klasses) do n = n + #klass.has end
  for _,klass in pairs(klasses) do
    tmp = i:like(#klasses,n)
    if tmp > most then most,liked = tmp,klass end end 
  return klass.txt, i:klass() end
--------------------------------------------------------------------------------
function COLS.new(i,t,    col)
  i.names, i.cols, i.xs, i.ys = t, {}, {}, {}
  for at,s in pairs(t) do
    col = push(i.cols, (s:find"^[A-Z]" and NUM or SYM)(at,s))
    col.goal = s:find"[!+-]$"
    if not s:find":$" then
      if s:find"!$" then i.klass = col end
      push(col.goal and i.ys or i.xs, col) end end end
 
function COLS.add(i,t) 
  for _,col in pairs(i.cols) do col:add(t[col.at]) end end
--------------------------------------------------------------------------------
function ROWS.new(i,src) i.has,i.cols = {},nil; return load(src,i) end

function ROWS.clone(i,ts,    j)
  j=ROWS({i.cols.names});for _,t in pairs(ts or {}) do j:add(t) end; return j end

function ROWS.add(i,t)
  if   i.cols then i.cols:add(push(i.has,t.cells and t or ROW(i,t)).cells) 
  else i.cols = COLS(t) end end
--------------------------------------------------------------------------------
function NB.new(i,src) i.first,i.all,i.klasses = true,ROWS(), {}; load(src,i) end

function NB.add(i,t,   k)
  i.all:add(t)
  k = (t.cells and t.cells or t)[i.all.cols.klass]
  if not i.first then i.klasses[k] = i.klasses[k] or i.all:clone()
                      i.klasses[k]:add(t) end
  i.first = false end
--------------------------------------------------------------------------------
local no,go = {},{}
function go.csv() for row in csv(THE.file)  do oo(row) end end
   
--------------------------------------------------------------------------------
if    pcall(debug.getlocal, 4, 1)
then  return {ROW=ROW, ROWS=ROWS, NUM=NUM, SYM=SYM, THE=THE,lib=lib} 
else  THE = cli(THE,help)
      demos(THE,go) end
