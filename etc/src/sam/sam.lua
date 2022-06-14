local help=[[
modules start with an Upper case letter
class methods are in Module.UPPERCASE (e.g. Module.NEW for constructors)
instance methods are in Module.method(i,...)
don't say self, say "i" (shorter)
where p-- osible, if looking at two instances, use "i,j"
types = int,real, str,tab,bool
]]
-------------------------------------------------------------------------------
-- ## Names

-- `the` stores settings for this code.
--  
-- - `Row` holds one record
-- - `Col` summarizes columns. One `Col` can be for
--   numerics or symbolic columns (denoted with ` aCol.nums`).
-- - `Data` holds many `Row`s, summarized in a table `aData.cols`
--   (where `aData.cols.x` holds independent columns and
--    `aData.cols.y` holds dependent columns). 
-- - `Bin` is a helper class that summarizes what dependent `ys` values are
--   found between `lo` and `hi` of an independent column.
require"lib"
local the={ Min  = .5,
            bins = 16,
            some = 256, 
            seed = 10019,
            wait = 10,
            m    = 2,
            k    = 1,
            file = "../../../data/auto93.csv"}

local Col,Data,Row,Bin = {},{},{},{}
-------------------------------------------------------------------------------
-- ## Col
-- Summaries a stream of numbers. Knows how to build the right kind of header
-- for each column. Knows how to store dependent columns separate to independent
-- columns.

--> .GOAL(names:[str]) :bool ->
--> .NUMP(names:[str]) :bool ->
--> .KLASS(names:[str]) :bool ->
--> .SKIP(names:[str]) :bool -> recognize different column types
function Col.GOAL(x)   return (x or ""):find"[!+-]$" end
function Col.NUMP(x)   return (x or ""):find"^[A-Z]" end
function Col.KLASS(x)  return (x or ""):find"!$"  end
function Col.SKIP(x)   return (x or ""):find":$"  end

--> .WEIGHT(names:[str]) :bool -> assign column weight (-1= minimize)
function Col.WEIGHT(x) return (x or ""):find"-$" and -1 or 1 end

--> .COLS(names:[str]) :COLS -> constructor: builds `Col`s from `names`.
function Col.COLS(names)
  local i={x={}, y={}, names=names, klass=nil}
  for at,txt in pairs(names) do
    local new = Col.NUMP(txt) and Col.NUM(at,txt) or Col.NEW(at,txt)
    if not Col.SKIP(txt) then
      push(Col.GOAL(txt) and i.y or i.x, new)
      if Col.KLASS(txt) then i.klass=new end end end
  return i end

--> .NEW(at:?int, txt:?str) :COL -> constructor of numbers
function Col.NEW(at,txt)
  return {n  =0,     at=at or 0, txt=txt or "", 
          ok =false, kept={},
          div=0,     mid=0} end

--> .NUM(at:?int, txt:?str) :COL -> constructor of numeric columns.
function Col.NUM(at,txt,some)
   i     = Col.NEW(at,txt)
   i.w   = Col.WEIGHT(txt)
   i.nums= some or the.some -- if non-nil the i.nums is a numeric
   return i end

--> .add(i:Col, v:any, inc:?int) :Col ->  update `i` with  `v ` ( inc  times)
function Col.add(i,v,inc)
 inc = inc or 1
 if   v ~= "?"
 then i.n = i.n + inc
      if i.nums 
      then for _=1,inc do
            if     #i.kept < i.nums then i.ok=false;push(i.kept,v) 
            elseif R() < i.nums/i.n then i.ok=false;i.kept[R(#i.kept)]=v end end 
      else i.ok = false
           i.kept[v] = inc + (i.kept[v] or 0) end end
  return i end

--> .ok(i:col) --> ensure that the current contents are up to date.
-- E.g. update `mid` (middle) with median
function Col.ok(i)
  if   not i.ok 
  then i.div, i.mid = 0, 0
       if   i.nums 
       then i.kept = sort(i.kept)
            i.mid  = per(i.kept, .5) -- median
            i.div  = (per(i.kept, .9) - per(i.kept, .1)) / 2.56 -- stdev
       else local most = -1 -- find the mode and ent
            for x,n in pairs(i.kept) do 
              if n > most then most, i.mid = n, x end
              i.div = i.div - n/i.n * math.log( n/i.n, 2) end end end 
  i.ok = true end
  
function Col.lo(i)   Col.ok(i); return i.kept[1] end
function Col.hi(i)   Col.ok(i); return i.kept[#i.kept] end
function Col.div(i)  Col.ok(i); return i.div end
function Col.mid(i)  Col.ok(i); return i.mid end
function Col.norm(i,x) 
  local lo,hi = Col.lo(i), Col.hi(i)
  return hi-lo < 1E-9 and 0 or (x-lo)/(hi-lo) end

function Col.bin(i,x)
  if   i.nums then
    local lo,hi = Col.lo(i), Col.hi(i)
    local b=(hi - lo)/the.bins
    x = lo==hi and 1 or math.floor(x/b+.5)*b end 
  return x end 

function Col.merge(i,j,     k)
  k = (i.nums and Col.NUM or Col.NEW)(i.at, i.txt)
  for _,kept in pairs{i.kept, j.kept} do
    for v,inc in pairs(kept) do Col.add(k,v,inc) end end
  return k end

-->.simpler(i:col,this:col,that:col):bool->am `i` simpler than `this` and `that`?
function Col.simpler(i,this,that)
  return Col.div(i) <= (this.n*Col.div(this) + that.n*Col.div(that)) / i.n end

function Col.like(i,x,prior)
  if   i.nums 
  then return ((i.kept[x] or 0)+the.m*prior)/(i.n+the.m) 
  else local sd,mu=Col.div(i), Col.mid(i)
       return sd==0 and (x==mu and 1 or 0) or
           math.exp(-1*(x - mu)^2/(2*sd^2)) / (sd*((2*math.pi)^0.5)) end end

--------------------------------------------------------------------------------
-- ## Row
function Row.NEW(of,cells) return {of=of,cells=cells,evaled=false} end

function Row.better(i,j)
  local s1, s2, ys = 0, 0, i.of.cols.y
  for _,c in pairs(ys) do
    local x,y =  i.cells[c.at], j.cells[c.at]
    x,y = Col.norm(c, x), Col.norm(c, y)
    s1  = s1 - 2.7183^(c.w * (x-y)/#ys)
    s2  = s2 - 2.7183^(c.w * (y-x)/#ys) end
  return s1/#ys < s2/#ys  end

function Row.klass(i) return i.cells[i.of.cols.klass.at] end
--------------------------------------------------------------------------------
-- ## Data
function Data.NEW(t) return {rows={}, cols=Col.COLS(t)} end

function Data.ROWS(src,fun)
  if type(src)=="table" then for  _,t in pairs(src) do fun(t) end
                        else for    t in csv(src)   do fun(t) end end end 

function Data.LOAD(src,    i)
  Data.ROWS(src,function(t) 
    if i then Data.add(i,t) else i=Data.NEW(t) end end); return i end
 
function Data.clone(i,inits,   j)
  j=Data.NEW(i.cols.names)
  for _,t in pairs(inits or {}) do Data.add(j,t) end; return j end

function Data.add(i,t)
  t = t.cells and t or Row.NEW(i,t)
  push(i.rows, t)
  for _,cols in pairs{i.cols.x, i.cols.y} do
    for _,c in pairs(cols) do Col.add(c, t.cells[c.at]) end end 
  return t end 

function Data.mids(i,cols, t) 
  t={}
  for _,c in pairs(cols or i.cols.y) do t[c.txt] = Col.mid(c) end;return t end

function Data.like(i,row, nklasses, nrows)
  local prior,like,inc,x
  prior = (#i.rows + the.k) / (nrows + the.k * nklasses)
  like  = math.log(prior)
  for _,col in pairs(i.cols.x) do
    x = row.cells[col.at] 
    if x and x ~= "?" then
      inc  = Col.like(col,x,prior)
      like = like + math.log(inc) end end
  return like end
--------------------------------------------------------------------------------
-- ## Bin
local NB={}
function NB.NEW(src,report)
  i  = {overall=nil, dict={}, list={}}
  Data.ROWS(src, function(row) 
    if not i.overall then i.overall = Data.NEW(row) else -- (0) eat row1
      row = Data.add(i.overall, row)  -- XX add to overall 
      if #i.overall.rows > the.wait then report(Row.klass(row), NB.guess(i,row)) end
      NB.train(i,row) end end)              -- add tp rows's klass
  return i end

function NB.train(i,row) 
  local k = Row.klass(row)
  i.dict[k] = i.dict[k] or push(i.list, Data.clone(i.overall)) -- klass is known
  i.dict[k].txt = k                            -- each klass knows its name
  Data.add(i.dict[k],row) end                  -- update klass with row

function NB.guess(i,row) 
  return argmax(i.dict, 
    function(klass) return Row.like(klass,row,#i.list,#i.overall.rows) end) end
--------------------------------------------------------------------------------
-- ## Bin
function Bin.NEW(xlo, xhi, ys) return {lo=xlo, hi=xhi, ys=ys} end
function Bin.add(i,x,y)
  i.lo = math.min(i.lo, x)
  i.hi = math.max(i.hi, x)
  Col.add(i.ys, y) end

function Bin.merge(i,j, min)
  local k = Col.merge(i,j)
  if i.n < min or j.n<min or Col.simpler(k,i,j)  then return k end end

function Bin.BINS(listOfRows,col,y)
  local n,list, dict = 0,{}, {}
  for label,rows in pairs(listOfRows) do
    for _,row in pairs(rows) do
      local v = row[col.at]
      if v ~= "?" then
        n = n + 1
        local pos = Col.bin(col,v)
        dict[pos] = dict[pos] or push(list, Bin.new(v,v,Col.new(col.at,col.txt)))
        Bin.add(dict[pos], v, label) end end end
    list = sort(list, lt"lo")
    list = col.nums and Bin.MERGES(list, n^the.min) or list
    return {bins= list,
            div = sum(list,function(z) return Col.div(z.ys)*z.ys.n/n end)} end
 
function Bin.MERGES(b4, min)
  local j,now = 1,{}
  while j <= #b4 do 
    local merged = j<#b4 and Bin.merge(b4[j], b4[j+1], min)
    now[#now+1]  = merged or b4[j] 
    j            = j + (merged and 2 or 1)  end
  if   #now < #b4 
  then return Bin.MERGES(now,min) -- loop to look for other merges
  else -- stretch the bins to cover minus infinity to plus Infinity
       for j=2,#now do now[j].lo = now[j-1].hi end
       now[1].lo, now[#now].hi = -big, big
       return now end end

--------------------------------------------------------------------------------
Go,No = {},{}

function Go.THE() oo(the) end

function Go.ROWS(  d) 
  Data.ROWS(the.file,function(row)
    if not d then d=Data.NEW(row) else
       Data.add(d,row) end end)
  oo(Data.mids(d)) end 

function Go.STATS() 
  oo(Data.mids(Data.LOAD(the.file) )) 
end

function Go.ORDER(  i,t) 
  i= Data.LOAD(the.file)
  t= sort(i.rows,Row.better)
  m=(#t)^.5
  left = Data.clone(i,splice(t,1,m))
  right= Data.clone(i,splice(i.rows,#t - m))
  print("all",  o(Data.mids(i)))
  print("best", o(Data.mids(left)))
  print("rest", o(Data.mids(right)))
  end 
--------------------------------------------------------------------------------
math.randomseed(the.seed)
if arg[1]=="-g" and type(Go[arg[2]])=="function" then Go[arg[2]]() end
