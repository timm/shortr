local the = {
             bins  = 16, 
             cohen = .35,
             file  = "../etc/data/auto93.csv",
             how   = "up",
             min   = .5
            }

local big = 1E32
local fmt = string.format

local function lt(x)       return function(a,b) return a[x] < b[x] end end 
local function push(t,x)   t[1+#t]=x; return x end
local function sort(t,f)   table.sort(t,f); return t end
local function map(t,f, u) u={};for k,v in pairs(t) do u[1+#u]=f(v) end; return u end

local function string2thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x  end

local function csv(csvfile) 
  csvfile = io.input(csvfile)
  return function(line, row) 
    line=io.read()
    if not line then io.close(csvfile) else
      row={}; for x in line:gmatch("([^,]+)") do push(row,string2thing(x)) end
      return row end end end 

local function o(t,    u)
  if #t>0 then return "{"..table.concat(map(t,tostring)," ").."}" else
    u={}; for k,v in pairs(t) do u[1+#u] = fmt(":%s %s",k,v) end
    return (t.is or "").."{"..table.concat(sort(u)," ").."}" end end
local function oo(t) print(o(t)) end

local function cli(d)
  for slot,x in pairs(d) do
    x = tostring(x)
    for n,flag in ipairs(arg) do 
      if flag=="--"..slot or flag=="-"..slot:sub(1,1) then
        x = x=="false" and"true" or x=="true" and"false" or arg[n+1] end end 
    d[slot] = string2thing(x) end 
  return d end

the = cli(the)

--------------------------------------------------------------------------------
local function is(name,    t,new)
  function new(kl,...) local x=setmetatable({},kl); kl.new(x,...); return x end 
  t = {__tostring=o, is=name or ""}; t.__index=t
  return setmetatable(t, {__call=new}) end

local SYM,NUM,COLS,ROW,ROWS = is"SYM", is"NUM", is"COLS", is"ROW", is"ROWS"

--------------------------------------------------------------------------------
local function use(s)    return s ~= "?" end
local function skip(s)   return s:find":$" end
local function klassp(s) return s:find"!$" end
local function nump(s)   return s:find"^[A-Z]" end
local function goalp(s)  return s:find"[!+-]$" end
local function weight(s) return s:find"-$" and -1 or 1 end

function SYM.new(i,c,s) i.n,i.at,i.txt,i.all = 0,c,s,{} end
function SYM.add(i,x) if use(x) then i.n=i.n+1;i.all[x]=(i.all[x] or 0)+1 end end 
function SYM.sub(i,x)                i.n=i.n-1;i.all[x]= i.all[x] -  1 end 
function SYM.val(i,goal,    b,r)
  b, r, goal = 0, 0, (goal==nil and true or goal)
  for x,n in pairs(i.all) do if x==goal then b=b+n else r=r+n end end
  return SYM.how[the.how]( b/(b+r+1/big),  r/(b+r+1/big)) end

SYM.how={}
function SYM.how.up(  b,r) return b+r < 0.05 and 0 or b^2/(b + r) end
function SYM.how.down(b,r) return b+r < 0.05 and 0 or r^2/(b + r) end
function SYM.how.away(b,r) return                      1 /(b + r) end

function NUM.new(i,c,s) i.at,i.txt,i.lo,i.hi,i.w=c,s,big,-big,weight(s) end
function NUM.add(i,x)   if use(x) then i.lo,i.hi=min(i.lo,x),max(i.hi,x) end end

local function _order(x) return type(x)=="number" and x or -1E32 end

function ROW.new(i,cells,egs) i.cells, i.egs = cells,egs end
function ROW.order(i,j,n)     return _order(i[n]) < _order(j[n]) end
function ROW.__lt(i,j)
  local y = i.of.cols.y
  local s1, s2, e = 0, 0,  math.exp(1)
  for _,col in pairs(y) do
     a  = col:norm(i.cells[col.at])
     b  = col:norm(j.cells[col.at])
     s1 = s1 - e^(col.w * (a - b) / #y)
     s2 = s2 - e^(col.w * (b - a) / #y) end
  return s1/#y < s2/#y end

function COLS.new(i,t)
  i.names, i.all, i.x, i.y, i.nums = t,{},{},{},{}
  for c,s in pairs(i.names) do 
    col = push(i.all, (nump(s) and NUM or SYM)(c,s))
    if not skip(s) then
      push(goalp(s) and i.y or i.x, col)
      if klassp(s) then i.klass = col end end end end
function COLS.add(i,cells)
  for at,col in pairs(i.all) do col:add(cells[at]) end end

function ROWS.new(i,src)
  i.rows, i.cols = {},nil
  if   type(src)=="table" 
  then for _,row in pairs(src) do i:add(row) end 
  else for   row in csv(src)   do i:add(row) end end end
function ROWS.add(i,row)
  if   i.cols 
  then i.cols:add( push(i.rows, row.cells and x or ROW(row,i)).cells )
  else i.cols = COLS(row) end end
function ROWS.copy(i,rows, j)
  j=ROWS({i.cols.names})
  for _,row in pairs(rows or {}) do j:add(row) end; 
  return j end

--------------------------------------------------------------------------------
function xys(col,yes,no,    x,out)
  out = {}
  for _,rk in pairs{{rows=yes, klass=true}, {rows=no, klass=false}} do
    for _,row in pairs(rk.rows) do 
      x= row.cells[col.at]
      if use(x) then push(out, {x=x, y=rk.klass}) end end end
  return out end

local _bins
function NUM.bins(i,t)
  t = sort(t, lt"x")
  return _bins(t, 1, #t, 0, (#t)^the.min,
                            (t[.9*#t//1] - t[.1*#t//1])/2.56*the.cohen) end

function _bins(t, lo, hi, n, min, epsilon)
  local lhs, rhs = SYM(), SYM()
  for j in lo,hi do rhs:add(t[j].y) end
  local x0, x1, best = t[1].x, t[#t].x, hi or rhs.all:val()
  local cut, x, y, z0, z1,down,up
  for j in lo,hi do
    x, y = t[j].x, t[j].y
    lhs:add(y)
    rhs:sub(y)
    if j-lo>min and hi-j+1> min and x-x0 > epsilon and x1-x > epsilon then
      if x ~= t[j+1].x then
        z0, z1 = lhs:val(), rhs:val() 
        if z0>best then best, down, up = z0, lo,   cut end
        if z1>best then best, down, up = z1, cut+1,hi  end end end end 
  return n<=1 and down and _bins(t, down, up, n+1, min, epsilon) or lo,hi,best end
