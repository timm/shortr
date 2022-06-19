local b4={}; for x,_ in pairs(_ENV) do b4[x]=x end 
local help= [[
  
SHORTR: semi-supervised multi-objective optimization XAI
(c) 2022 Tim Menzies <timm@ieee.org> BSD2 license
   
From N items, find and explain the best ones, using just log(N) evals.
PASS1 (guess): eval two distant items on multi-objective criteria.
      Prune everything nearest the worst one. Recurse on rest.  
PASS2 (guess again): do it again, using better items from first pass.  
PASS3 (explain): recursively discretize attributes on how well they
      distinguish the best and worst items (seen in second pass).

USAGE:
  lua shortr.lua [OPTIONS]

OPTIONS:
  -M  --Min    min size of space                    =  .5
  -b  --bins   max number of bins                   =  16
  -k  --k      Bayes hack: low attribute frequency  =  2
  -m  --m      Bayes hack: low class frequency      =  1
  -s  --some   max number of nums to keep           =  256
  -w  --wait   wait this number before testing      =  10

OPTIONS (other):
  -f  --file   file           =  data/auto93.csv
  -g  --go     start-up goal  =  nothing
  -h  --help   show help      =  false
  -s  --seed   seed           =  10019]]

-------------------------------------------------------------------------------
-- ## Names

-- `Row` holds one record (in `cells`) and a pointer (`_of`) to 
-- the container that holds it `container that made them.<p>
local Row={} 
-- `Col` summarizes columns. One `Col` can be for
--   numerics or symbolic columns (denoted with ` aCol.nums`).<p>
local Col={} or {n=0}
-- `Data` holds many `Row`s, summarized in a table `aData.cols`
--   (where `aData.cols.x` holds independent columns and
--    `aData.cols.y` holds dependent columns). <p>
local Data={}
-- `Bin` is a helpe class that summarizes what dependent `ys` values are
--   found between `lo` and `hi` of an independent column.<p>
local Bin={}
-- `NB` is an application class that implements a Naive Bayes classifier.
local NB={}
-- Imports  
local _    = require"lib"
local lib  = _
local Abcd = require"abcd"
-- `the` stores settings for this code.
local the={}
help:gsub(" [-][-]([^%s]+)[^\n]*%s([^%s]+)",function(key,x) the[key] =_.atom(x) end)

-- If `the.Min` is greater than one, then `small` is `the.Min`. 
-- Else it is some fractional power of some number.
local function small(n) return the.Min <1 and n^the.Min or the.Min end

-- Other names
local argmax,atom,big,cli,csv,demos = _.argmax,_.atom,_.big,_.cli,_.csv,_.demos
local fmt,id,lt,map,o,oo,per,push   = _.fmt,_.id,_.lt,_.map,_.o,_.oo,_.per,_.push
local R,rnd,sort,splice,sum             = _.R, _.rnd,_.sort, _.splice, _.sum

-------------------------------------------------------------------------------
-- ## class Col
-- Summaries a column of data. Uses different types for numeric or other data.
-- It `i.nums` is non-nil, then this is a column for numerics.

--> .NEW(at:?int, txt:?str) :Col -> constructor of columns.
-- `.ok` is set to false after every update then set back
-- to true if ever we update the columns (see `Col.ok`).
function Col.NEW(at,txt)
  return {n  =0,         --> :num    -> number of items seen so far    
          at=at or 0,    --> :num    -> column number
          txt=txt or "", --> :str    -> name of this column
          ok =false,     --> :bool   -> true if  derived contents up to date
          kept={},       --> :[any]  -> summary of the items seen so far
          div=0,         --> :number -> diversity (sd,entropy for nums.other)
          mid=0 } end    --> :any    -> middle (median,mode for nums,other)

--> .NUM(at:?int, txt:?str) :Col -> constructor, specialized for numerics.
-- Numbers have a weight (-1,1) as well as the manddate to keep 
-- no more than  `aNum.nums` samples.
function Col.NUM(at,txt,some,   i)
   i     = Col.NEW(at,txt)  --> :[COL] -> numerics extend general columns.
   i.w   = Col.WEIGHT(txt)  --> :num   -> if minimizing, then -1. Else 1
   i.nums= some or the.some --> :num   -> max num of items to keep. 
   return i end
-- ### Factory to make Cols

--> .GOAL(x:[str]) :bool ->
--> .NUMP(x:[str]) :bool ->
--> .KLASS(x:[str]) :bool ->
--> .SKIP(x:[str]) :bool -> recognize different column types
function Col.GOAL(x)   return (x or ""):find"[!+-]$" end
function Col.NUMP(x)   return (x or ""):find"^[A-Z]" end
function Col.KLASS(x)  return (x or ""):find"!$"  end
function Col.SKIP(x)   return (x or ""):find":$"  end

--> .WEIGHT(x:[str]) :(-1|1) -> assign column weight.e.g. "-1" means "minimize",
function Col.WEIGHT(x) return (x or ""):find"-$" and -1 or 1 end

--> .COLS(names:[str]) :tab -> constructor (builds `Col`s from list of `names`).
-- Returns a table that stores dependents in `.y`, independents in `.x`, 
-- the klass (if it exists)in `.klass`. Caveat: 
-- only if we are not `.SKIP()`ping them.
function Col.COLS(names)
  local i={x={}, y={}, names=names, klass=nil}
  for at,txt in pairs(names) do
    local new = Col.NUMP(txt) and Col.NUM(at,txt) or Col.NEW(at,txt)
    if not Col.SKIP(txt) then
      push(Col.GOAL(txt) and i.y or i.x, new)
      if Col.KLASS(txt) then i.klass=new end end end
  return i end
-- ### Update

--> .add(i:Col, v:any, inc:?int) :Col ->  update `i` with  `v ` ( inc  times)
-- Numeric columns keep a sample of the numbers while other columns track the
-- frequency of symbols seen so far.  The larger the sample, the less often
-- we update the numerics.
function Col.add(i,v,inc)
 inc = inc or 1
 if   v ~= "?"
 then i.n = i.n + inc
      if i.nums 
      then for __=1,inc do
            if     #i.kept < i.nums then i.ok=false;push(i.kept,v) 
            elseif R() < i.nums/i.n then i.ok=false;i.kept[R(#i.kept)]=v end end 
      else i.ok = false
           i.kept[v] = inc + (i.kept[v] or 0) end end
  return i end
-- ### Computing derived properties

--> .ok(i:Col) -> ensure that the current contents are up to date. Returns `kept`.
-- E.g. update `mid`dle and `div`ersity (_median_ and _standard deviation_ for
-- numerics; and _mode_ and _entropy_ for others).<p> To understand the idiom
-- "(per(.9) - per(.1))/2.56", recall that &pm;1 and &pm;2 standard deviations
-- selects 66 to  95% of the mass.  In between (at &pm;1.28), we get to 90%.  So to
-- find 1 standard deviation, divide 90th - 10th percentile by twice 1.28 (2.56).
function Col.ok(i)
  if   not i.ok 
  then i.div, i.mid = 0, 0
       if   i.nums 
       then i.kept = sort(i.kept) -- very fast since "kept" is small
            i.mid  = per(i.kept, .5) -- median
            i.div  = (per(i.kept, .9) - per(i.kept, .1)) / 2.56 -- stdev
       else local most = -1 -- find the mode and ent
            for x,n in pairs(i.kept) do 
              if n > most then most, i.mid = n, x end
              if n > 0 then i.div=i.div - n/i.n*math.log(n/i.n,2) end end end end 
  i.ok = true 
  return i.kept end
-- ### Querying
-- Most of these need to call `Col.ok()` first (to ensure column is up to date).

--> .lo(i:Col) :num -> 
--> .hi(i:Col) :num -> 
--> .div(i:Col) :num -> 
--> .mid(i:Col) :any ->  `lo`west number, `hi`ghest number, `div`ersity, `mid`dle number. 
function Col.lo(i)   Col.ok(i); return i.kept[1] end
function Col.hi(i)   Col.ok(i); return i.kept[#i.kept] end
function Col.div(i)  Col.ok(i); return i.div end
function Col.mid(i,p) Col.ok(i); return rnd(i.mid,p) end

--> .norm(i:Col,x:num) :0..1 -> normalize `x` 0..1 for lo..hi.
function Col.norm(i,x) 
  local a=Col.ok(i); return a[#a]-a[1] < 1E-9 and 0 or (x-a[1])/(a[#a]-a[1]) end
-- ### For Discretization 

--> .bin(i:Col,x:any) :any -> round numeric `x` to nearest `(hi-lo)/the.bins`
-- (and for non-numerics, just return `x`).
function Col.bin(i,x)
  if  not i.nums then return x end
  local a = Col.ok(i)
  local b = (a[#a] - a[1])/the.bins
  return a[#a]==a[1] and 1 or math.floor(x/b+.5)*b end 

--> .bin(i:Col,j:Col) :Col -> returns a combination of two columns.
function Col.merge(i,j,     k)
  k = (i.nums and Col.NUM or Col.NEW)(i.at, i.txt)
  for _,kept in pairs{i.kept, j.kept} do
    for v,inc in pairs(kept) do Col.add(k,v,inc) end end
  return k end

-->.simpler(i:col,this:col,that:col):bool->am `i` simpler than `this` and `that`?
function Col.simpler(i,this,that)
  return Col.div(i) <= (this.n*Col.div(this) + that.n*Col.div(that)) / i.n end
-- ### For Naive Bayes 

function Col.like(i,x,prior)
  if  not i.nums then return ((i.kept[x] or 0)+the.m*prior) / (i.n+the.m) else
    local sd,mu=Col.div(i), Col.mid(i)
    if sd==0 then return x==mu and 1 or 1/big end
    return math.exp(-.5*((x - mu)/sd)^2) / (sd*((2*math.pi)^0.5)) end  end

--------------------------------------------------------------------------------
-- ## Row
function Row.NEW(of,cells) 
  return {id     = id(),      --> :num    -> unique id 
          _of    = of,        --> :[DATA] -> pointer back to creating containing
          cells  = cells,     --> :[any]  -> row values
          evaled = false} end --> :bool   -> true if we use this rows' "y" values

function Row.better(i,j)
  i.evaled, j.evaled = true, true
  local s1, s2, ys = 0, 0, i._of.cols.y
  for _,c in pairs(ys) do
    local x,y =  i.cells[c.at], j.cells[c.at]
    x,y = Col.norm(c, x), Col.norm(c, y)
    s1  = s1 - 2.7183^(c.w * (x-y)/#ys)
    s2  = s2 - 2.7183^(c.w * (y-x)/#ys) end
  return s1/#ys < s2/#ys  end

function Row.klass(i) return i.cells[i._of.cols.klass.at] end
--------------------------------------------------------------------------------
-- ## Data
function Data.NEW(t) return {
   rows={},              --> :[Row]   -> rows being stored here
   cols=Col.COLS(t)} end --> :[[Col]] -> info and summaries about columns

function Data.ROWS(src,fun)
  if type(src)=="table" then for  _,t in pairs(src) do fun(t) end
                        else for    t in csv(src)   do fun(t) end end end 

function Data.LOAD(src,    i)
  Data.ROWS(src,function(t) 
    if i then Data.add(i,t) else i=Data.NEW(t) end end); return i end
 
function Data.clone(i,inits)
  local j=Data.NEW(i.cols.names)
  for _,t in pairs(inits or {}) do Data.add(j,t) end; return j end

function Data.add(i,t)
  t = t.cells and t or Row.NEW(i,t)
  push(i.rows, t)
  for _,cols in pairs{i.cols.x, i.cols.y} do
    for _,c in pairs(cols) do Col.add(c, t.cells[c.at]) end end 
  return t end 

function Data.mids(i,p,cols,    t) 
  t={};for _,c in pairs(cols or i.cols.y) do t[c.txt]=Col.mid(c,p) end;return t end
-- ### For Naive Bayes 

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
-- ### For Decision Trees 

function Data.tree(i,listOfRows)
  local total,rows,ylabels = 0,{},{}
  for label,rows1 in pairs(listOfRows) do 
    total = total + #rows
    for _,row in pairs(rows1) do 
      rows[1+#rows]=row
      ylabels[row.id]=label end 
  end ----- end data collection
  local y,mapSortedBins
  function y(row) 
    return ylabels[row.id] end 
  function mapSortedBins(j) 
    local bins,down
    function bins(xcol) 
      return Bin.BINS(j.rows,xcol,y,Col.New) end
    function down(bin)
      local new = Data.clone(j, Bin.holds(bin, j.rows))
      if #new.rows<#j.rows then 
        new.gaurd=bin 
        return mapSortedBins(new) end 
    end ----------------------
    if #j.rows >= 2*small(total) then 
      j.kids = map(sort(map(j.cols.x, bins),lt"div")[1].bins, down) end
    return j 
  end ------
  return mapSortedBins(Data.clone(i, rows)) end

function Data.show(i,lvl)
  lvl = lvl or 0
  local gaurd = i.gaurd and Bin.show(i.gaurd)
  print(fmt("%-40s", o(Data.mids(i,2))), ("| "):rep(lvl) .. (gaurd or ""))
  for _,kid in pairs(i.kids or {}) do Data.show(kid, 1+lvl) end end
  
--------------------------------------------------------------------------------
-- ## NB
function NB.NEW(src,report)
  local i  = {overall=nil,  --> :[Data] -> summary of all rows
              dict={},      --> :[Data] -> rows, indexed by klass name
              list={}}      --> :[Data] -> a list of items in "dict"
  report = report or print
  Data.ROWS(src, function(row) 
    if not i.overall then i.overall = Data.NEW(row)  else -- (0) eat row1
      row = Data.add(i.overall, row)  -- XX add to overall 
      if #i.overall.rows > the.wait then report(Row.klass(row), NB.guess(i,row)) end
      NB.train(i,row) end end)              -- add tp rows's klass
  return i end

function NB.train(i,row) 
  local kl = Row.klass(row)
  i.dict[kl] = i.dict[kl] or push(i.list, Data.clone(i.overall)) -- klass is known
  i.dict[kl].txt = kl                            -- each klass knows its name
  Data.add(i.dict[kl],row) end                  -- update klass with row

function NB.guess(i,row) 
  return argmax(i.dict, 
    function(klass) return Data.like(klass,row,#i.list,#i.overall.rows) end) end
--------------------------------------------------------------------------------
-- ## Bin
function Bin.NEW(xlo, xhi, ys) 
   return {lo=xlo,     --> :num   -> low x
           hi=xhi,     --> :num   -> high x
           ys=ys} end  --> :[any] -> y values seen for "lo" to "hi"

function Bin.show(i)
  local x,lo,hi = i.ys.txt, i.lo, i.hi
  if     lo ==  hi  then return fmt("%s == %s",x, lo)  
  elseif hi ==  big then return fmt("%s >= %s",x, lo)  
  elseif lo == -big then return fmt("%s < %s", x, hi)  
  else                   return fmt("%s <= %s < %s",lo,x,hi) end end

function Bin.add(i,x,y)
  i.lo = math.min(i.lo, x)
  i.hi = math.max(i.hi, x)
  Col.add(i.ys, y) end

function Bin.merge(i,j, min)
  local iy,jy = i.ys,j.ys
  local ky    = Col.merge(iy,jy)
  if iy.n < min or jy.n<min or Col.simpler(ky,iy,jy) then 
    return Bin.NEW(i.lo, j.hi, ky) end end

function Bin.hold(i, row)
  local x = row.cells[i.ys.at]
  return x=="?" or i.lo==i.hi or i.lo<x and x<=i.hi end

function Bin.holds(i, rows)
  return map(rows,function(row) if Bin.hold(i,row) then return row end end) end 

function Bin.BINS(rows,col,y,yKlass)
  y      = y or function(row) return Row.klass(row) end 
  yKlass = yKlass or Col.NEW
  local n,list, dict = 0,{}, {}
  for _,row in pairs(rows) do
    local v = row.cells[col.at]
    if v ~= "?" then
      n = n + 1
      local pos = Col.bin(col,v)
      dict[pos] = dict[pos] or push(list, Bin.NEW(v,v,yKlass(col.at,col.txt)))
      Bin.add(dict[pos], v, y(row)) end end 
  list = sort(list, lt"lo")
  list = col.nums and Bin.MERGES(list, small(n)) or list
  return {bins= list,
          div = sum(list,function(z) return Col.div(z.ys)*z.ys.n/n end)} end
 
function Bin.MERGES(b4, min)
  local n,now = 1,{}
  while n <= #b4 do 
    local merged = n<#b4 and Bin.merge(b4[n], b4[n+1], min)
    now[#now+1]  = merged or b4[n] 
    n            = n + (merged and 2 or 1)  end
  return #now < #b4 and Bin.MERGES(now,min) or Bin.XPAND(now) end

-- xpand the bins to cover any gaps from minus infinity to plus infinity
function Bin.XPAND(bins)
  if #bins>1 then
    for n=2,#bins do bins[n].lo = bins[n-1].hi end end
  bins[1].lo, bins[#bins].hi = -big, big
  return bins end 
--------------------------------------------------------------------------------
-- To disable a test, relabel it from `Go` to `No`.
local Go,No = {},{}

function Go.TREE(  i,t,m,left,right)
  i = Data.LOAD(the.file)
  t = sort(i.rows,Row.better)
  m = #t/2
  left  = splice(t,1,m)
  right = splice(i.rows,#t - m)
  local n,labels,rows = 0,{},{}
  local y = function(row) return labels[row.id] end 
  print""
  Data.show(Data.tree(i,{left,right}))
  return true end

function Go.BINS(  i,t,m,left,right)
  i = Data.LOAD(the.file)
  t = sort(i.rows,Row.better)
  m = (#t)^.5
  left  = splice(t,1,m)
  right = splice(i.rows,#t - m)
  local n,labels,rows = 0,{},{}
  local y = function(row) return labels[row.id] end 
  for label,rows1 in pairs({left,right}) do 
    n = n + #rows
    for m,row in pairs(rows1) do 
      rows[1+#rows] = row
      labels[row.id]=label end end
  for n,xcol in ipairs(i.cols.x) do
    print("")
    local bins = Bin.BINS(rows, xcol, y, Col.new).bins
    if #bins > 1 then 
       for _,bin in pairs(bins) do
         print(bin.ys.txt, bin.lo, bin.hi) end end end
  return true end 

function Go.THE() oo(the); return true end

function Go.ROWS(  d) 
  Data.ROWS(the.file,function(row)
    if not d then d=Data.NEW(row) else
      Data.add(d,row) end end)
  oo(Data.mids(d))
  oo(Col.ok(d.cols.x[1]))
  return true end 

function Go.STATS() 
  oo(Data.mids(Data.LOAD(the.file) )); return true end

function Go.ORDER(  i,t,m,left,right) 
  i= Data.LOAD(the.file)
  print(0, lib._id)
  t= sort(i.rows,Row.better)
  m= (#t)^.5
  left = Data.clone(i,splice(t,1,m))
  right= Data.clone(i,splice(i.rows,#t - m))
  print(2, lib._id)
  print("all",  o(Data.mids(i)))
  print("best", o(Data.mids(left)))
  print("rest", o(Data.mids(right))) 
  return true end 

function Go.DIABETES(f,  i,t,a) 
  a = Abcd.NEW()
  NB.NEW(f or "data/diabetes.csv",function(x,y) Abcd.add(a,x,y) end) 
  Abcd.pretty(a,Abcd.report(a)) 
  return true end

function Go.SOYBEAN()  return Go.DIABETES("data/soybean.csv") end

--------------------------------------------------------------------------------
if    pcall(debug.getlocal, 4, 1)
then  return {DATA=DATA,ROW=ROW, COL=COL, the=the,lib=lib} 
else  the = cli(the,help)
      demos(the,Go) end 
