-- vim: ts=2 sw=2 et : 
-- ego.lua : simple landscape analysis (code that is "conscious" of shape of data)
-- (c) 2022 Tim Menzies.  Usage of the works is permitted provided that this
-- instrument is retained with the works, so that any entity that uses the works
-- is notified of this instrument. DISCLAIMER:THE WORKS ARE WITHOUT WARRANTY.  

local help=[[
ego.lua: landscape analysis (being 'conscious' of shape of data)
(c) 2022 Tim Menzies, timm@ieee.org     
"Don't you believe what you've seen or you've heard, 
 'ego' is not a dirty word" ~ Greg Macainsh

INSTALL:
  requires: lua 5.4+
  download: etc.lua, ego.lua, egs.lua
  test    : lua egs.lua -h

USAGE:
  lua egs.lua [OPTIONS]

OPTIONS:                                    default
                                            -------
  -A  --Also  rest is 'also'*Best           = 3
  -B  --Best  use #t^Best as 'best'         = .5
  -b  --bins  max bins for numeric          = 16
  -G  --Goal  goal;  one of: up,down,over   = up
  -k  --keep  #numerics to keep per column  = 256
  -s  --seed  random number seed            = 10019

OPTIONS (other):
  -f  --file  csv file with data            = ../etc/data/auto93.csv
  -h  --help  show help                     = false
  -g  --go    start up action               = nothing ]]

local etc=require"etc"
local big,cli,csv,fmt         =etc.big, etc.cli, etc.csv, etc.fmt
local is,lt,map,o,o,push      =etc.is,  etc.lt,  etc.map, etc.o, etc.oo,etc.push
local splice,sort,string2thing=etc.splice, etc.sort, etc.string2thing
local the = {}
--------------------------------------------------------------------------------
local SOME,NUM,SYM,ROWS = is"SOME", is"NUM", is"SYM", is"ROWS"

local function merge(ranges,min,       a,b,ab,j,n,tmp)
  if ranges[1].x.is == "SYM" then return ranges end
  j,n,tmp = 1,#ranges,{}
  while j<=n do 
    a, b = ranges[j], ranges[j+1]
    if b then 
      ab = a.y:clone():inject(a.y,b.y)
      if a.n<min or b.n<min or ( 
         ab:div() < (a.y:div()*a.y.n + b.y:div()*b.y.n)/ab.n)
      then a = {x=a.x:clone():inject(a.x,b.x),   y=y}
           j = j+1 end end
    tmp[#tmp+1] = a
    j = j+1 end
  if #tmp < 2       then return {} end           -- distribution has no splits
  if #tmp < #ranges then return merge(tmp,min) end
  for j=2,#tmp do tmp[j].x.lo = tmp[j-1].x.hi end -- fill in any gaps
  tmp[1].x.lo, tmp[#tmp].x.hi = -big, big         -- stretch across all numbers
  return tmp end  

--------------------------------------------------------------------------------
function SYM.new(i,at,name) i.n,i.txt,i.at,i.has = 0,txt or "",at or 0,{} end
function SYM.add(i,x,inc)
  inc = inc or 1
  if x~="?" then i.n = i.n+inc; i.has[x]= inc+(i.has[x] or 0) end end

function SYM.clone(i) return SYM(i.at,i.txt) end
function SYM.inject(i,...)
  for _,more in pairs{...} do for x,n in pairs(more.has) do i:add(x,n) end end
  return i end

function SYM.div(i, e)
  e=0;for _,v in pairs(i.has) do if n>0 then e=e-v/i.n*math.log(v/i.n,2) end end
  return e end

function SYM.range(i,x) return x end

function SYM.want(u,goal,B,R,how,   b,r,z)
  local how={
    good= function(b,r) return ((b<r or b+r < .05) and 0) or b^2/(b+r) end,
    bad=  function(b,r) return ((r<b or b+r < .05) and 0) or r^2/(b+r) end,
    novel=function(b,r) return 1/(b+r) end}
  b, r, z  = 0, 0, 1/big
  goal = goal~=nil and goal or true
  for x,n in pairs(i.has) do
    if x==goal then b=b+n else r=r+n end end
  return how[the.Goal or "good"](b/(B+z), r/(R+z)) end

function SYM.select(i,t)   x=t[i.at]; return x=="?" or i.has[x] end
--------------------------------------------------------------------------------
function SOME.new(i) i.has, i.ok, i.n = {}, false,0 end
function SOME:all() if not i.ok then sort(i.has) end;i.ok=true; return i.has end
function SOME.add(i,x)
  i.n = 1 + i.n
  print(1,x)
  if     #i.has < the.keep     then i.ok=false; push(i.has,x)  
  elseif rand() < the.keep/i.n then i.ok=false; i.has[rand(#i.has)]=x end end 
--------------------------------------------------------------------------------
function NUM.new(i,at,txt) 
  i.n,i.mu,i.m2,i.sd,i.txt,i.at = 0,0,0,0,txt or "",at or 0
  i.w,i.lo,i.hi,i.has          = i.txt:find"-$" and -1 or 1,big,-big,SOME() end

function NUM.add(i,x,   d)
  if x~="?" then 
    i.has:add(x)
    i.n  = i.n+1
    d    = i.mu - x
    i.mu = i.mu + d/i.n
    i.m2 = i.m2 + d*(x - i.mu)
    i.sd = (i.n<2 or i.m2<0) and 0 or (i.m2/(i.n-1))^0.5 
    i.lo = math.min(x, i.lo)
    i.hi = math.max(x, i.hi) end end

function NUM.clone(i) return NUM(i.at,i.txt) end
function NUM.inject(i,...)
  for _,more in pairs{...} do for _,n in pairs(more.has.has) do i:add(n) end end
  return i end

function NUM.div() return i.sd end

function NUM.norm(i,x) 
  return (x=="?" and x) or (i.hi-i.lo<1E-9 and 0) or (x-i.lo)/(i.hi-i.lo) end

function NUM.range(i,x,n,   b) b=(i.hi-i.lo)/n; return math.floor(x/b+0.5)*b end
function NUM.select(i,t) x=t[i.at]; return x=="?" or i.lo <= x and x <= i.hi end 
--------------------------------------------------------------------------------
function ROWS.new(i, src)
  i.names, i.has, i.cols, i.x, i.y = {}, {}, {}, {}, {}
  if type(src)=="table"
  then for _,row in pairs(src) do i:add(row) end
  else for   row in csv(  src) do i:add(row) end end end

function ROWS.add(i,row)
  if    #i.names > 0 
  then  push(i.has,row)
        for _,col in pairs(i.cols) do col:add(row[col.at]) end 
  else  i.names = row
        for at,txt in pairs(row) do 
          local col = push(i.cols, (txt:find"^[A-Z]" and NUM or SYM)(at,txt))
          if not txt:find":$" then
            if txt:find"!$" then i.klass=col end
            push(txt:find"[!+-]$" and i.y or i.x, col) end end end end 

function ROWS.betters(i)
  return sort(i.has, function(r1,r2) 
                       local s1,s2,e,y,a,b = 0,0,math.exp(1),i.y
                       for _,col in pairs(y) do
                         a,b = col:norm(r1[col.at]), col:norm(r2[col.at])
                         s1 = s1 - e^(col.w * (a - b) / #y)
                         s2 = s2 - e^(col.w * (b - a) / #y) end
                       return s1/#y < s2/#y end) end

function ROWS.xx1(col,yklass,j,y,seen)
  x=i.has[j][col.at]
  if x~="?" then 
    bin= col:range(x)
    seen[bin] = seen[bin] or {x=col:clone(), y=yklass()}
    seen[bin].x:add(x)
    seen[bin].y:add(y) end end

function ROWS.xx(i)
  i.rows = i:betters()
  n = (#i.has)^the.Best
  step = (#i.has - n1)/(the.Also*n1)
  for _,col in pairs(i.x) do
    tmp={}
    for j=1,n,1            do i:xx1(col,SYM,j,true, tmp) end
    for j=n+1,#i.rows,step do i:xx1(col,SYM,j,false,tmp) end end end
--------------------------------------------------------------------------------
return {SOME=SOME,NUM=NUM,SYM=SYM,ROWS=ROWS,help=help}
