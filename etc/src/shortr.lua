-- <span id="forkongithub"><a href="https://github.com/timm/l5">Fork me on GitHub</a></span>
-- <a
-- href="https://github.com/timm/l5/actions/workflows/tests.yml"><img 
-- src="https://github.com/timm/l5/actions/workflows/tests.yml/badge.svg"></a><br><a 
-- href="https://zenodo.org/badge/latestdoi/206205826">
-- <img src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a>
-- 
-- Do I understand "it"?
-- Can I code "it" succinctly? Using simple tools and a minimal set of data 
-- structure and algorithms? Does "it" do the minimum work  (so"it" runs fast)?
--  Can I explain "it" to you, quickly and successfully? And does "it" generalize
-- (so "it" is not a one-off hack)?<p>Let's check. Please review my code.   
-- This is  a multi-objective semi-supervised explanation tool,
-- all in under 400 lines of code
--  (most of which is background stuff that could be used for other learners).    
-- 
-- `awk '!/^(--|[ \t]*$)/{n++} END{print n" lines"}' *.lua`  
-- => 367 lines
--
-- Share and enjoy!<hr>  
--  
-- __Mother Teresa:__   
-- <img width=175 align=left src=cup.png> 
-- "The more you have, the more you are occupied.
-- The less you have, the more free you are."<p>
-- __Ken Thompson:__     
-- "One of my most productive days was throwing 
-- away 1,000 lines of code."<p>
-- __William of Occam:__   
-- "It is vain to do with more what can be done with less."  <p>
-- __Donald Knuth:__  
-- "It is much more rewarding to do more with less." <p>
-- __Leonardo da Vinci:__    
-- "Simplicity is the ultimate sophistication."<p>
-- __Edsger D. Dijkstra:__    
-- "Simplicity is prerequisite for reliability."<p>
-- __Dieter Ram:__    
-- "Less, but better."<p>
-- __timm:__    
-- "plz, less"<p>
-- Hall of fame (whose code challenges us to write less, but better):   
-- [Jack Diederich](https://www.youtube.com/watch?v=o9pEzgHorH0) 
-- | [Hilary Mason](https://boingboing.net/2017/06/30/next-level-regexp.html)
-- | [Brian Kernighan](https://www.oreilly.com/library/view/beautiful-code/9780596510046/ch01.html)  
--   [Peter Norvig](http://norvig.com/lispy.html)
-- | [Joel Grus](https://github.com/joelgrus/data-science-from-scratch)
-- | [Paul Graham](http://www.paulgraham.com/onlisp.html)
--<p>   
local help= [[

SHORTR: semi-supervised multi-objective optimization
(c) 2022 Tim Menzies <timm@ieee.org> BSD2 license

Explore N points via  O(log2(N)) evaluations. Generate a
human-readable summary of that space.  In pass1, find and
eval  two distant points using multi-objective criteria.
Everything nearest the  worst is pruned and we recurse on
the rest.  This algorithm is only approximate so, in pass2,
we do it all again, starting with the better items seen in
pass1.  Explain the final results by  a decision tree that
recursively discretizes numerics via their ability to
distinguish the best/worst things found in pass2.

USAGE:
  lua shortr.lua [OPTIONS]

OPTIONS:
  -F  --Few    only keep a "Few" numbers  =  256
  -S  --small  small leaf size            =  .5
  -b  --Bins   max number of bins         =  16
  -k  --k      handle rare classes        =   1
  -m  --m      handle rare attributes     =   2
  -p  --p      distance coefficient       =   2
  -w  --wait   wait before classifying    =  10

OPTIONS (other):
  -f  --file   file           =  ../../data/auto93.csv
  -g  --go     start-up goal  =  nothing
  -h  --help   show help      =  false
  -s  --seed   seed           =  10019]]
--------------------------------------------------------------------------------
-- ## Names
local _ = require"lib"
local argmax,big                     = _.argmax,_.big
local cli,csv,demos,klass,normpdf    = _.cli,_.csv,_.demos,_.klass, _.normpdf
local oo,push,rand,read,rnd,same,str = _.oo,_.push,_.rand,_.read,_.rnd,_same,_.str

-- `THE` settings is parsed from `help` 
-- string lines that contain two dashes&nbsp;"`--`".
local THE={}
help:gsub(" [-][-]([^%s]+)[^\n]*%s([^%s]+)",function(key,x) THE[key] = read(x) end)
-- ### Our classes   

-- - ROWS use COLS to make either NUMs or SYMs.  
-- - ROWS holds data in ROWs, and summarizes columns in NUMs and SYMs.  
-- - There are two helper classes:
--   - NUMs use SOMEs to keep up to `THE.Few` nums per columns.   
--   - RANGE objects track what `y` was seen from `xlo` to `xhi`.
-- - NB is a bayes classifier built from these sub-routunes.
local ROWS, COLS, NUM, SYM = klass"ROWS",  klass"COLS",  klass"NUM", klass"SYM"
local ROW = klass"ROW"
local SOME,RANGE = klass"SOME", klass"RANGE"
local NB = klass"NB"
--------------------------------------------------------------------------------
-- ## class COLS

-- The system uses COLS to make lists of NUMs or SYMs.
--    
--  Independent and dependent columns are stored in `xs` or `ys` lists.<p>
-- This code reads data from csv files or lists of tables.
-- The first row of data is a list of column names. 
--  
-- Those can contain some special symbols.
-- For example, we want to ignore columns whose name ends with "`:`".
-- Also, number columns start with an upper case letter.
-- Further, dependent variables end in "`!`", "`+`", or "`-`" 
-- for klasses or goals to be minimized or maximized.
local  is={}
function is.use(x)      return not x:find":$" end
function is.num(x)      return x:find"^[A-Z]" end
function is.goal(x)     return x:find"[!+-]$" end
function is.klass(x)    return x:find"!$"     end
function is.minimize(x) return x:find"-$"     end

-- For example, `COLS` would read line1 of the following data, skipping column #2, making
-- NUMerics out of columns #1,#2, and a SYMbolic out of columni #4. Further,
-- any goal columns (in this case, "`Lbs-`")  get stored in a `ys` list
-- and all other columns are stored in `xs`.
--
--     Clndrs,  Hp:,  Lbs-,  origin
--     8,       193,  4732,  1
--     8,       215,  4615,  1
--     8,       200,  4376,  1
--     ...

-- __COLS(  `t`  :[string] )__<br>constructor.
function COLS.new(i,t,     new)
  i.xs, i.ys, i.names = {},{},t
  for at,txt in pairs(t) do
    new = (is.num(txt) and NUM or SYM)()
    new.txt,  new.at = txt, at
    new.usep, new.w  = is.use(txt), (is.minimize(txt) and -1 or 1)
    if new.usep  then 
      if is.klass(new.txt) then i.klass=new end
      push(is.goal(new.txt) and i.ys or i.xs, new) end end end

-- __.add(  `t`  :ROW  )  :ROW__<br>update column summaries.
function COLS.add(i,t)
  for _,cols in pairs{i.xs,i.ys} do
    for _,col in pairs(cols) do col:add(t.cells[col.at]) end end
  return t end
--------------------------------------------------------------------------------
-- ## class SOME
-- NUMs use SOMEs to store at most `THE.Few` samples per numeric columns. 

-- __SOME()__<br>constructor.
function SOME.new(i) i.n,i.t,i.ok = 0,{},true end
-- __.add(  `x`:any  )__<br>
-- Technically, this is a reservoir sampler; i.e. given a small cache,
-- sample at an equal (but low) probability across  a much larger  space.
-- Note one trick: If we full, make space by replacing anything at all.
function SOME.add(i,x)
  if x=="?" then return x end
  i.n = i.n + 1 
  if     #i.t   < THE.Few     then i.ok=false; push(i.t,x)  
  elseif rand() < THE.Few/i.n then i.ok=false; i.t[rand(#i.t)]=x end end 
-- __.all()  :table__<br>If we ask for `.all()`, then make sure they are sorted.
function SOME.all(i) i.t=i.ok and i.t or sort(i.t); i.ok=true; return i.t end
--------------------------------------------------------------------------------
-- ## class NUM
-- Summarize a stream of numbers, maintaining its `lo`, `hi`, `mu` (mean).

-- __NUM()__<br>constructor.  
function NUM.new(i) i.n,i.mu,i.m2,i.w,i.lo,i.hi,i.some=0,0,0,1,big,-big,SOME() end
--  __.add(  `v`  :number  )__<br>add `v` to the summary, updating `mu,sd,lo,hi`.
function NUM.add(i, v)
  if v=="?" then return v end
  i.some:add(v)
  i.n  = i.n + 1
  local d    = v - i.mu
  i.mu = i.mu + d/i.n
  i.m2 = i.m2 + d*(v - i.mu)
  i.sd = i.n<2 and 0 or (i.m2/(i.n-1))^0.5 
  i.lo = math.min(v, i.lo) 
  i.hi = math.max(v, i.hi) end 
 --  __.bin(  `x`  :number  )  :number__<br>return x, rounded into `THE.bins` values.
function NUM.bin(x) 
  b=(i.hi - i.lo)/THE.bins; return i.lo==i.hi and 1 or math.floor(x/b+.5)*b end
-- __.like(  `x`  :number  )  :number__<br>return likelihood.   
function NUM.like(i,x,...) return normpdf(x, i.mu, i.sd) end
--  __.merge(  `j`  :NUM  )  :NUM__<br>Return a NUM that combines self and `j`.
function NUM.merge(i,j,      k)
  local k = NUM(i.at, i.txt)
  for _,n in pairs(i.some.t) do k:add(x) end
  for _,n in pairs(j.some.t) do k:add(x) end
  return k end
-- __.mid(  ?`p`=2  )  :number__<br>report rounded middle.  
function NUM.mid(i,p) return rnd(i.mu,p) end
--------------------------------------------------------------------------------
-- ## class SYM

-- Summarize a stream of symbols, maintaining the `mode` and frequencies counts
-- of symbols seen so far.

-- __SYM()__<br>constructor.  
function SYM.new(i)  i.n,i.syms,i.most,i.mode = 0,{},0,nil end
--  __.add(  `v`  :any,  ?`inc`=1   )__<br>add `v`, `inc` number of times.
function SYM.add(i,v,inc)
  if v=="?" then return v end
  inc=inc or 1
  i.n = i.n + inc
  i.syms[v] = inc + (i.syms[v] or 0)
  if i.syms[v] > i.most then i.most,i.mode = i.syms[v],v end end
 --  __.bin(  `x`  :any  )  :any__<br>discrediting a symbol just returns that symbol.
function SYM.bin(x) return x end
-- __.like(  `x`  :any  )  :number__<br>report likelihood.   
function SYM.like(i,x,prior) return ((i.syms[x] or 0)+THE.m*prior)/(i.n+THE.m) end
--  __.merge(  `j`  :SYM  )  :SYM__<br>Return a SYM that combines self and `j`.
function SYM.merge(i,j,      k)
  local k = SYM(i.at, i.txt)
  for x,n in pairs(i.has) do k:add(x,n) end
  for x,n in pairs(j.has) do k:add(x,n) end
  return k end
-- __.mid()  :any__<br>return the  most commonly seen symbol.
function SYM.mid(i,...)  return i.mode end
--------------------------------------------------------------------------------
-- ## class ROW
-- Store data from one record. Track if we ever use this ROW's dependent variables.
-- Hold a pointer back to a ROWS (so we can access attribute positions and
-- weightings).

-- __ROW(  `of`  :ROWS,  `cells`   :table)__<br>constructor.  
function ROW.new(i,of,cells) i.of,i.cells,i.evaled  = of,cells,false end
-- __.klass()__<br>Return the klass variable of this ROW.
function ROW.klass(i)    return i.cells[i.of.cols.klass.at] end
-- __.within(  `range`  :RANGE  )   :boolean__<br>True if ROW is in `range`.
function ROW.within(i,range)
   local lo, hi, at = range.xlo, range.xhi, range.ys.at
   local v = i.cells[at]
   return  v=="?" or (lo==hi and v==lo) or (lo<v and v<=hi) end
--------------------------------------------------------------------------------
-- ## class ROWS

-- Store some ROWs. Keep a summary of the columns inside `cols`. Keep the
-- dependent and independent summaries seepage in `cols.ys` and `cols.xs`.

-- __ROWS(  `t`  :[string]  )__<br>constructor.
function ROWS.new(i,t) i.cols=COLS(t); i.rows={} end
-- __.add(  `t`  :(table|ROW)  )  :ROW__<br>update with a table or ROW.
function ROWS.add(i,t) return push(i.rows, i.cols:add(t.cells and t or ROW(i,t))) end
-- __.mid(  `cols`  :[NUM|SYM],  `p`=2  )  :table__   
-- returns `mid`s of some columns; round numerics to `p` decimal places.
function ROWS.mid(i, cols, p,     t)
  t={};for _,col in pairs(cols or i.cols.ys) do t[col.txt]=col:mid(p) end;return t end
-- __.clone(  ?`data`  :(table|[ROW])  )  :ROWS__<br>copy this structure, maybe add data.
function ROWS.clone(i,data,  j) 
  j= ROWS(i.cols.names);for _,row in pairs(data or {}) do j:add(row) end; return j end
-- __.like(  `row`  :ROWS,  `nklasses`  :int;  `nrows`  :int  )  :number__  
-- how likely is it that `row` could live here?
function ROWS.like(i,row, nklasses, nrows,    prior,like,inc,x)
  prior = (#i.rows + THE.k) / (nrows + THE.k * nklasses)
  like  = math.log(prior)
  for _,col in pairs(i.cols.xs) do
    x = row.cells[col.at] 
    if x and x ~= "?" then
      inc  = col:like(x,prior)
      like = like + math.log(inc) end end
  return like end
-- __doRows(  ?`src`  :(string|table),  `fun`  :function( table|ROW )   )__   
-- helper function for reading from tables or files. Case arg1 of ...     
-- ... _table_  : call  function for all items in table.  
-- ... _string_ :  call function on all rows from a file.     
-- ... _nil_    :  call function of all rows from standard input.
local function doRows(src, fun)
  if type(src)=="table" then for  _,t in pairs(src) do fun(t) end
                         else for   t in csv(src)   do fun(t) end end end
-- ## Decision (or Regression) Tree

-- __.tree(  `listOrRows`  :[[ROW]] )  :ROW__   
-- return root of decision or regression tree;
function ROWS.tree(i, listOrRows)
  local ylabels = {}
  for label,rows in pairs(listOfRows) do
    for _,row in pairs(rows) do ylabels[ push(all,row).id ] = label end end
  return i:treeGrow(i.cols.xs, function(row) return ylabels[row.id] end) end
-- __.treeGrow(  `xcols`  :[NUM|SYM],  `y`  :function  )  :ROW__   
-- return root of decision or regression tree;
function ROWS.treeGrow(i,xcols,y,         gaurd,stop,all,some)
  rows.gaurd = gaurd
  stop = stop or (#i.rows)^THE.small
  rows.kids = {}
  if #i.rows >= 2*stop then
    all = map(xcols, function(xcol) return RANGE.make(i.rows,xcol,SYM,y) end)
    for j,range in pairs(sort(all, lt"div")[1].ranges) do 
      some = i:clone()
      for _,row in pairs(i.rows) do
        if row:within(range) then some:add(row); break end end 
      if #some.rows < #i.rows then
        i.kids[j] = some:treeGrow(xcols, y, range, stop) end end end end
-- __.show(  `pre`  :string,   ?`show`  :function)__<br>pretty print;
function ROWS.show(i,pre,show,        show0)
  function show0(i) print(fmt("%s%s%s" , pre, i.gaurd, #i.rows)) end
  pre = pre or ""
  (show or show0)(i)
  for _,kid in pairs(i.kids or {}) do kid:show(pre.."|.. ", show) end end
--------------------------------------------------------------------------------
-- ## class NB
-- (0) Use row1 to initial our `overall` knowledge of all rows.   
-- After that (1) add row to `overall` and (2) ROWS about this row's klass.       
-- (3) After `wait` rows, classify row BEFORE updating training knowledge
function NB.new(i,src,report,             row)
  i.overall, i.dict, i.list  = nil, {}, {}
  doRows(src, function(row,   k) 
    if not i.overall then i.overall = ROWS(row) else -- (0) eat row1
      row = i.overall:add(row)                       -- add to overall 
      if #i.overall.rows > THE.wait then report(row:klass(), i:guess(row)) end
      i:train(row) end end) end                      -- add tp rows's klass

function NB.train(i,row,      k) 
  k = row:klass()
  i.dict[k] = i.dict[k] or push(i.list,i.overall:clone()) -- klass is known
  i.dict[k].txt = k                            -- each klass knows its name
  i.dict[k]:add(row) end                       -- update klass with row

function NB.guess(i,row) 
    return argmax(i.dict, 
      function(klass) return klass:like(row,#i.list,#i.overall.rows) end) end
--------------------------------------------------------------------------------
-- ## class RANGE
--  RANGE objects track what `y` was seen from `xlo` to `xhi`. 

-- __RANGE(  `xlo`  :number,  `xhi`  :number,  `yes`  :(NUM|SYM)  )__<br>constructor;
function RANGE.new(i, xlo, xhi, ys) i.xlo,i.xhi,i.ys,i.rows = xlo,xhi,ys,{} end

-- __RANGE(  `x`  :number,  `y`  :number  )__<br>extend range to cover `x`;   
-- and add `y` to the y-values seen so far.
function RANGE.add(i,x,y)
  if x < i.xlo then i.xlo = x end -- works for string or num
  if x > i.xhi then i.xhi = x end -- works for string or num
  i.ys:add(y) end

-- __.\_\_tostring()  :string__<br>pretty print;
function RANGE.__tostring(i)
  local x, lo, hi = i.ys.txt, i.xlo, i.xhi
  if     lo ==  hi  then return fmt("%s == %s",x, lo)  
  elseif hi ==  big then return fmt("%s > %s",x, lo)  
  elseif lo == -big then return fmt("%s <= %s", x, hi)  
  else                   return fmt("%s < %s <= %s",lo,x,hi) end end

-- __.merge(  `i`  :RANGE,  `j`  :RANGE,  `min`  :integer)  :(RANGE|nil)__   
-- Merge ranges that are too small (less than `min`), or are more complex
-- than their combination. Return nil otherwise. Note: should only be applied
-- to _adjacent_ ranges.
function RANGE.merge(i,j,min)
  local lo,hi       = i.xlo, j.xhi
  local y1, y2, y12 = i.ys, j.ys, i.ys:merge(j.ys)
  local n1, n2, n12 = y1.n, y2.n, y12.n
  if n1<min or n2<min or y12:div() <= (n1*y1:div() + n2*y2:div()) / n12 then 
    return RANGE(lo, hi, y12)  end end
   
-- ## Discretization (making ranges)

-- __RANGE.make(  `rows`  :[ROW],  `xcol`  :(NUM|SYM),<br>`yclass`:KLASS,  `y`  :function  )  :table__  
-- Class method. Returns a list `{ranges={RANGE},  div=number}` that scores each range.  
-- Numeric ranges are first divided into groups of size `(hi-lo)/THE.bins` 
-- then subsequently, adjacent ranges are merged (see `_merges`). 
-- Finally, each range is score by the weighted sum of its diversity.
--     
-- _Low-level detail:_ About the idiom:   
-- `dict[pos]=dict[pos] or push(list,RANGE(...))`            
-- The `map` and `sort` functions used in this code
-- do funny things when sorting associative arrays
-- with discontinuous keys. 
-- To have something that can reliably sorted, while at the same time, have a fast way to access one item,
-- this code maintains   
-- (1) a "fast-find" index (`dict`)   
-- (2) something with continuous indexes (`list`).
local _merges, _xpand
function RANGE.make(rows,xcol,yklass,y)
  local n,list, dict = 0,{}, {}
  for _,row in pairs(rows) do
    local v = row.cells[xcol.at]
    if v ~= "?" then
      n = n + 1
      local pos = xcol:bin(v)
      dict[pos] = dict[pos] or push(list, RANGE(v,v, yklass(xcol.at, xcol.txt)))
      dict[pos]:add(v, y(row)) end end 
  list = sort(list, lt"xlo")
  list = xcol.is=="NUM" and _xpand(_merges(list, n^THE.min)) or list
  return {ranges= list,
          div   = sum(list,function(z) return z.ys:div()*z.ys.n/n end)} end

-- __\_merge( `b4`  :[RANGE],   `min`:number  )  :[RANGE]__   
-- Try to merge adjacent ranges. If successful, then loop looking for other merges.
function _merges(b4, min)
  local j,now = 1,{}
  while j <= #b4 do 
    local merged
    if j<#b4 then merged = b4[j]:merge(b4[j+1], min) end
    now[#now+1] = merged and merged or b4[j] 
    j           = j + (merged and 2 or 1) end-- skip to next and next if we found a merge
  return  #now == #b4 and now or _merges(now,min) end -- hunt for more merges

-- __\_xpand(  `ranges`  :[RANGE]  )  :[RANGE]__   
-- Expand the ranges to cover everything from minus to plus infinity.
function _xpand(ranges)
  for j=2,#ranges do ranges[j].xlo = ranges[j-1].xhi end
  ranges[1].xlo, ranges[#t].xhi = -big, big
  return ranges end
--------------------------------------------------------------------------------
-- ## Tests
local no,go = {},{}
function go.the()  print(1); print(THE); return type(THE.p)=="number" and THE.p==2 end

function go.argmax(  t,fun)
  fun=function(x) return -x end
  t={50,40,0,40,50}
  return 3 == argmax(t,fun) end

function go.num(n) n=NUM(); for x=1,100 do n:add(x) end; return n.mu==50.5 end

function go.sym(s) 
  s=SYM(); for _,x in pairs{"a","a","a","a","b","b","c"} do s:add(x) end
  return s.mode=="a" end

function go.csv(    n,s) 
  n,s=0,0; for row in csv(THE.file)  do n=n+1; if n>1 then s=s+row[1] end end
  return rnd(s/n,3) == 5.441  end

function go.rows(    rows)
  doRows(THE.file,function(t) if rows then rows:add(t)  else rows=ROWS(t) end end) 
  return rnd(rows.cols.ys[1].sd,0)==847 end

local function _classify(file) 
  local Abcd=require"abcd"
  local abcd=Abcd()
  NB(file, function(got,want) abcd:add(got,want) end)
  abcd:pretty(abcd:report())
  return true end

function go.soybean()  return _classify("../../data/soybean.csv") end 
function go.diabetes() return _classify("../../data/diabetes.csv") end 
--------------------------------------------------------------------------------
-- ## Start
if    pcall(debug.getlocal, 4, 1)
then  return {doRows=doRows,ROW=ROW, ROWS=ROWS, NUM=NUM, SYM=SYM, THE=THE,lib=lib} 
else  THE = cli(THE,help)
      demos(THE,go) end
