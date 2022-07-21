--- --- --- --- Runnings
local l=require"lib"
local x=require"xplan"

local any,big,cat,chat,coerce,cp2n,csv= l.any, l.big, l.cat,  l.chat, l.coerce, l.cp2n, l.csv
local fmt,kap,lt,map,max,min     = l.fmt, l.kap, l.lt,   l.map,  l.max,    l.min
local obj,per,pers,push,rand,rnd,rnds = l.obj, l.per, l.pers,l.push, l.rand, l.rnd,    l.rnds
local shuffle,sort,slice,sum           = l.shuffle, l.sort, l.slice, l.sum 

local the,COLS,NUM,SYM,ROWS = x.the, x.COLS, x.NUM, x.SYM, x.ROWS
--- --- --- Demos
--- --- Storage
-- Disable 
local go,no= {},{}

function go.the() chat(the); return true end
function go.map() 
  chat(map({1,2,3}, function(x) return 10*x end)); return true end

function go.rnds()
  chat(rnds({a=10.1111,b=20.2222},2)); return true end

function go.sort()
  return sort({{x=10, y=20}, {x=30,y=10}},lt"y")[1].x == 30 end

function go.csv()
  the.file = "../../data/auto93.csv"
  local n=0
  csv(the.file,function(row) 
    n=n+#row 
    if n>9 then assert(type(row[1])=="number") end end) 
  return n>1 and n % 8 == 0 end

function go.sym(  s)
  s=SYM(); for _,x in pairs{"a","a","a","a","b","b","c"} do s:add(x) end
  return s:mid()=="a" and rnd(s:div(),2) == 1.38  end

function go.num(  n)
  n=NUM(); for i=1,100 do n:add(i) end 
  return 50==n:mid() and 31.25==rnd(n:div(),2) end

function go.cols()
  chat(
    COLS({"Clndrs","Volume","Hp:","Lbs-","Acc+","Model","origin","Mpg+"}).all) end

function go.rows( rs)
  rs = ROWS():file(the.file)
  chat(rs:mids())
  return 101.95 == rnd(rs.cols.all[2]:div(),2) end

function go.clone(   rs1,rs2,c2)
  rs1 = ROWS():file(the.file)
  rs2 = rs1:clone(rs1.rows) 
  local ok=true
  for n,c1 in pairs(rs1.cols.x) do c2=rs2.cols.x[n]; ok = ok and c1:div()==c2:div() end 
  for n,c1 in pairs(rs1.cols.y) do c2=rs2.cols.y[n]; ok = ok and c1:div()==c2:div() end 
  return ok end

function go.dist(  r1,r2,rs)
  rs = ROWS():file(the.file)
  local ok=true
  for i = 1,250 do 
    r1, r2 = any(rs.rows), any(rs.rows)
    local d= rs.cols:dist(r1,r2)
    ok = ok and d>=0 and d<=1 end
  return ok end

function go.half(    rs,half)
  rs = ROWS():file(the.file)
  half = rs.cols:half(rs.rows) 
  print(#half.As, #half.Bs)
  return rs.cols:dist(half.A, half.B) > .5 end

function go.bests(    rs,bests,rests,n,evals)
  rs = ROWS():file(the.file)
  print("before",cat(rs:mid()))
  for i=1,20 do 
    rs = ROWS():file(the.file)
    rs.rows = rs.cols:rank(rs.rows)
    bests,rests,evals = rs.cols:bests(rs.rows) 
    print(evals,
          rnd(cp2n(.95,the.cohen/6),2),
          cat(sort(map(bests,function(r) return r.rank end))))
  end
  return true end 

function go.ranks(    rs)
  rs = ROWS():file(the.file)
  rs.cols:rank(rs.rows) 
  for _,row in pairs(rs.rows) do print(row.rank) end 
  return true end

function go.splitter(  rs)
  rs=ROWS():file(the.file)
  rs.rows=sort(rs.rows, function(r1,r2) return rs.cols:best(r1,r2) end) 
  local rows={}
  for _,row in pairs(slice(rs.rows, 1, 30))       do push(rows,row).label=0 end
  for _,row in pairs(slice(rs.rows, #rs.rows-30)) do push(rows,row).label=1 end
  for _,bin in pairs(rs:splitter(rows)) do print(bin) end
  return true end

function go.tree(    rs)
  ROWS():file(the.file):tree()
  for _,row in pairs(rs.rows) do print(row.rank) end 
  return true end

--- --- --- Start-up
--- Setting up to run
the = l.cli(the,x.help)         -- update "the" from command-line
local reset = {} 
for k,v in pairs(the) do reset[k]=v end -- squirrel away a copy of "the"
local all   = l.sort(l.kap(go,function(k,_) return k end))
local steps = the.go=="all" and all or {the.go}

-- Run. Reset "the" and random seed before each step.
-- Any step that does not return true will increment the `fail` count
local fails=0
for _,step in pairs(steps) do
  if type(go[step])=="function" then
    for k,v in pairs(reset) do the[k]=v end 
    math.randomseed(the.seed)
    if true ~= go[step]() then 
      print("FAIL:",step)
      fails=fails+1 end end end 

l.rogues()
os.exit(fails) 
