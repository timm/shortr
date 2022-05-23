local help=[[  
LOOK: landscape analysis 
(c) 2022 Tim Menzies, timm@ieee.org, BSD2 license 
"I think the highest and lowest points are the important ones. 
 Anything else is just... in between." ~Jim Morrison

INSTALL: requires: lua 5.4+
         download: lib.lua, look.lua, looking.lua
         test    : lua egs.lua -h

USAGE: lua looking.lua [OPTIONS]
                                      defaults
                                      --------
  --also  -a  size of rest=best*also  = 4
  --p     -p  distance coefficient    = 2
  --far   -f  far                     = .95 
  --Some  -S  sample size             = 256
  --seed  -s  random number seed      = 10019
  --min   -m  min size pass1          = .5
  --Min   -M  min size pass2          = 10

  --file  -f  csv file with data      = ../../etc/data/auto93.csv
  --help  -h  show help               = false
  --loud  -l  verbose mode            = false
  --go    -g  start up action         = nothing]]

local _ = require"lib"
local any,big,csv,is,lt,many,map = _.any, _.big, _.csv, _.is, _.lt, _.many, _.map
local o,oo,push,shuffle,sort     = _.o, _.oo, _.push, _.shuffle, _.sort
local tothing                    = _.tothing

local the={}
help:gsub(" [-][-]([^%s]+)[^\n]*%s([^%s]+)",function(k,x) the[k]=_.tothing(x)end)
--------------------------------------------------------------------------------
local ROW=is"ROW"
function ROW.new(i,of,cells) i.cells, i.of, i.evaluated = cells,of,false end 
function ROW.__lt(i,j,        n,s1,s2,v1,v2)
  n,s1,s2 = 0,0,0
  for _,__ in pairs(i.of.ys) do n = n + 1 end
  for c,w in pairs(i.of.ys) do
    v1,v2 = i.of:norm(c, i.cells[c]), i.of:norm(c, j.cells[c])
    s1    = s1 - 2.7183^(w * (v1 - v2) / n)
    s2    = s2 - 2.7183^(w * (v2 - v1) / n) end
  return s1/n < s2/n end

function ROW.dist(i,j,     d,n,dist1)
  function dist1(c,v1,v2)
    if v1=="?" and v2=="?" then return 0 end
    if not i.of.nums[c] 
    then return v1==v2 and 0 or 1 
    else if     v1=="?" then v2=i.of:norm(c,v2); v1= v2<.5 and 1 or 0 
         elseif v2=="?" then v1=i.of:norm(c,v1); v2= v1<.5 and 1 or 0 
         else   v1,v2 = i.of:norm(c,v1), i.of:norm(c,v2) end
         return math.abs(v1-v2) end 
  end ---------------------------
  d,n = 0,0
  for c,_ in pairs(i.of.xs) do 
    n,d = n+1, d + (dist1(c,i.cells[c], j.cells[c]))^the.p end
  return (d/n)^(1/the.p) end
--------------------------------------------------------------------------------
local ROWS=is"ROWS"
local function num(s)  return s:find"^[A-Z].*" end
local function goal(s) return s:find"[!+-]$" end
local function wght(s) return s:find"-$" and -1 or 1 end 

function ROWS.new(i,src) 
  i.rows, i.nums, i.xs, i.ys, i.names =  {},{},{},{},nil
  if type(src)=="table" then for _,r in pairs(src) do i:add(r) end
                        else for   r in csv(  src) do i:add(r) end end end

function ROWS.clone(i,inits,    j)
  j=ROWS({i.names}); for _,r in pairs(inits or {}) do j:add(r) end; return j end

function ROWS.add(i,t,     r)
  if   i.names 
  then r = t.cells and r or ROW(i,t); i:update(r.cells); push(i.rows, r) 
  else i:header(t) end end

function ROWS.header(i,t)
  i.names = t
  for c,s in pairs(t) do if num(s) then i.nums[c]={lo=big,hi=-big} end end 
  for c,s in pairs(t) do if goal(s)then i.ys[c]=wght(s) else i.xs[c]=c end end end

function ROWS.update(i,t,   v)
  for c,num in pairs(i.nums) do
    v = t[c]
    if v ~="?" then num.lo = math.min(v, num.lo)
                    num.hi = math.max(v, num.hi) end end end

function ROWS.norm(i,c,v,   lo,hi)
  lo,hi = i.nums[c].lo, i.nums[c].hi
  return (v=="?" and v) or ((hi-lo) < 1E-9 and 0) or (v-lo)/(hi-lo) end

function ROWS.around(i,r1,t,          fun)
  function fun(r2) return {dist=r1:dist(r2), row=r2} end
  return sort(map(t or i.rows, fun), lt"dist") end

function ROWS.far(i,r1,t,   tmp)
  tmp= i:around(r1,t)
  return tmp[(#tmp)*the.far//1].row end

function ROWS.mid(i,cols)
  local function mid(c,t)
    if i.nums[c] then
      local s,n,v = 0,0
      for _,r in pairs(i.rows) do v=r[c]; if v~="?" then n=n+1;s=s+r[c] end end 
      return s/n 
    else 
      local most,mode,tmp,v = 0,nil,{}
      for _,r in pairs(i.rows) do 
        v=r[c]; if v~="?" then tmp[v] = 1 + (tmp[v] or 0) end end
      for x,n in pairs(tmp) do if n>most then mode,most = x,n end end
      return mode end 
   end --------------
   out={}; for c,_ in pairs(cols or i.ys) do out[c] = mid(c,i.rows) end
   return out end 

function ROWS.look(i,  w,sample,best,rests)
  w      = i.rows
  sample = many(w, the.Some)
  best   = i:far(any(sample), sample)
  rests  = {}
  for _,stop in pairs({2*(#w)^the.min, the.Min})  do
    while #w > stop do
      local rest = i:far(best, sample)
      if rest < best then best,rest = rest,best end
      best.evaluated, rest.evaluated = true,true
      local c = best:dist(rest)
      for _,r in pairs(w) do r.x=(r:dist(best)^2 +c^2- r:dist(rest)^2)/(2*c) end 
      local bests = {}
      for n,r in pairs(sort(w,lt"x")) do push(n<=#w/2 and bests or rests,r) end 
      if #bests==#w then break else w=bests end
      sample = many(w,the.Some) end end
 return ra,w,many(rests, #w*the.also) end

return {ROWS=ROWS, ROW=ROW, help=help, the=the}
