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
local function map_xy(at,yes,no,fun)
  for _,rowsy in pairs{{rows=yes, y=true}, {rows=no, y=false}} do
    for _,row in pairs(rowsy.rows) do 
      if use(row.cells[at]) then fun(row.cells[at],rowsy.y) end end end end

function SYM.bestBin(i,yes,no,   all,tmp,fun)
  local function fun(x,y) do
    tmp[x] = tmp[x] or push(all,SYM())
    tmp[x]:add(y) end
  all,tmp = {},{}
  map_xy(i.at,yes,no,fun)
  best = sort(all, function(a,b) return a:val() > b:val() end)[1]
  return best.x, best.x, best:val() end

function NUM.bestBin(i,yes,no,   fun, t)
  all,t=SYM(),{}
  map_xys(i.at,yes,no, function(x,y) all:add(y); push(t,{x=x,y=y}) end)
  t = sort(t,lt"x")
  return _numbins(t, 1, #t, all, (#t)^the.min,
                               (t[.9*#t//1] - t[.1*#t//1])/2.56*the.cohen) end

function _numbins(t, lo, hi, rhs, min, epsilon)
  local lhs = SYM()
  local x0, x1, best = t[lo].x, t[hi].x, hi or rhs.all:val()
  local so,ok,x, y, down, up = copy(rhs),false
  for j in lo,hi do
    x, y = t[j].x, t[j].y
    lhs:add(y)
    rhs:sub(y)
    if j-lo>min and hi-j+1> min and x-x0 > epsilon and x1-x > epsilon then
      if x ~= t[j+1].x then
        l,r = lhs:val(), rhs:val() 
        if l>best then ok,best,down,up,so=true,l,lo,  j,copy(lhs) end
        if r>best then ok,best,down,up,so=true,l,j+1,hi copy(rhs) end end end end 
   if   ok 
   then return _numbins(t, down, up, so, min, epsilon) 
   else return t[lo].x, t[hi].x,so end end
