--- --- --- --- Runnings
local l=require"lib"
local x=require"xplan"

local any,big,cat,chat,coerce,csv= l.any, l.big, l.cat,  l.chat, l.coerce, l.csv
local fmt,kap,lt,map,max,min     = l.fmt, l.kap, l.lt,   l.map,  l.max,    l.min
local obj,per,push,rand,rnd,rnds = l.obj, l.per, l.push, l.rand, l.rnd,    l.rnds
local shuffle,sort,sum           = l.shuffle, l.sort, l.sum 

local the=x.the
local COLS,NUM,SYM,ROWS = x.COLS, x.NUM, x.SYM, x.ROWS
--- --- --- Main 
---  main(tab,tab) -- Runs some (or all) of the demos. Return number of failures.
-- Resets `the` and the random number seed before each demo. 
function l.main(the, go, help) 
  local the, fails, defaults = l.cli(the,help), 0, {}
  for k,v in pairs(the) do defaults[k]=v end 
  local todos = l.sort(l.kap(go,function(k,_) return k end))
  for _,todo in pairs(the.go=="all" and todos or {the.go}) do
    if type(go[todo])=="function" then
      for k,v in pairs(defaults) do the[k]=v end 
      math.randomseed(the.seed)
      if true ~= go[todo]() then 
        print("FAIL:",todo)
        fails=fails+1 end end end 
  l.rogues()
  os.exit(fails) end

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

function go.half(    rs,two)
  rs = ROWS():file(the.file)
  two = rs.cols:half(rs.rows) 
  for _,row in pairs(rs.rows) do chat(row) end
  for _,row in pairs(two.As) do chat(row) end
  for _,row in pairs(two.Bs) do chat(row) end
  return rs.cols:dist(two.A, two.B) > .5 
  end

function go.bests(    rs,bests,rests)
  rs = ROWS():file(the.file)
  chat(rs:mid())
  bests,rests = rs.cols:bests(rs.rows) 
  chat(rs:clone(bests):mid()) end

--- --- --- Start-up
l.main(x.the, go, x.help)
