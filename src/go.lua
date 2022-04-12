local R = require
local _, the, ABCD  = R"lib", R"the", R"ABCD"
local NUM, SYM, BIN,EGS,COLS,RULE = R"num",R"sym",R"bin",R"egs",R"cols",R"rule"
local per,map,dent = _.per, _.map, _.dent

local ish,copy,items,o,oo,powerset = _.ish,_.copy,_.items,_.o,_.oo,_.powerset
local map,fmt,rnds, rnd,push       = _.map,_.fmt,_.rnds, _.rnd,_.push
local class,Obj = _.class, _.Obj
local no,go,ok     = _.no, _.go,_.ok

function go.class()
  local EMP=class("EMP",Obj)
  function EMP:show() return {"name", "age", "_id"} end
  function EMP:new(name) self._id=1;  self.name=name; self.age=0 end 
  local fred = EMP("tim")
  local MANAGER=class("MANAGER",EMP)
  local jane = MANAGER("jane")
  print(jane) end

function go.copy(     t,u)
  t={a={b={c=10},d={e=200}}, f=300}
  u= copy(t) 
  t.a.b.c= 20
  ok(u.a.b.c ~= 20,"copy") end

function go.rnd()
  ok("23.11" == rnds({23.11111})[1],"rounds") end

function go.collect()
  local function aux(x,y) return x*y end
  oo(_.collect({10,20,30},aux)) end

function go.items()
  for  x in items{10,20,30} do oo(x) end 
  local n=0
  for x in items(the.file) do n=n+1; if n<=5 then oo(x) end end end

function go.powerset()
  for _,x in pairs(powerset{10,20,30,40,50}) do oo(x) end end
 
 function go.many( t)
  local o,many=_.o,_.many
  t={};for j = 1,1000 do t[#t+1] = j end
  print(900,"+", o(many(t, 10, 900)))
  print(1,100,   o(many(t, 10,   1, 100)))
  print(300,700, o(many(t, 10, 300, 700))) end 

function go.some( n)
  the.some=512
  n=NUM()
  for i=1,999 do n:add( i//100) end
  for k,v in pairs(SYM():adds(n:all()).has) do print(k,v) end end

function go.ent()
  local n = SYM()
  n:add("a",9)
  n:add("b",7)
  ok(ish(n:div(), .98886), "entropy")  end

function go.normal( n)
  n=NUM()
  for i=1,10^3 do n:add( _.normal(10,2) //1) end
  for n,k in pairs(SYM():adds(n:all()).has) do print(n,k) end end

function go.nums( n)
  n=NUM()
  for i=1,10^2 do n:add(_.normal(8,1)) end
  oo(rnds{n:mid(), n:div()}) end

function go.cols()
  _.dent(COLS{"Name","Age:","gender","Weight-"}) end

function go.egs(  i)
  i= EGS():adds(the.file)
  ok(7   == i.cols.x[2].has["lt40"], "counts")
  ok(286 == #i.rows,"egs") end

function go.clone(    i,j)
  i= EGS():adds("../etc/data/auto93.csv")
  j= i:clone(i.rows) 
  local flag = true
  for k,n in pairs(i.cols.y[1]:all()) do 
    flag=flag and n==j.cols.y[1]:all()[k] end
  ok(flag,"clone") end

function go.mid(     all,best,rest)
  all       = EGS():adds("../etc/data/auto93.csv")
  best,rest = all:bestRest()
  best      = all:clone(best)
  rest      = all:clone(rest)
  print("all ",o(all:mid()))
  print("best",o(best:mid()))
  print("rest",o(rest:mid())) end

function go.bins(     all,best,rest,b4)
  all       = EGS():adds("../etc/data/auto93.csv")
  best,rest = all:bestRest()
  best      = all:clone(best)
  rest      = all:clone(rest)
  for _,bin in pairs(best:bins(rest)) do
    if bin.at ~= b4 then print("") end
    print(bin.name, bin.at,bin.lo,bin.hi, 
          bin.ys.has["left"] or 0, 
          bin.ys.has["right"] or 0) 
    b4 = bin.at end end

local function _rules(file,     all,bests,rests,left,right,b4,bins,rules,h)
  all       = EGS():adds(file)
  print(o(rnds(all:mid())), o(rnds(map(all:div(),function(x)return x*the.cohen end))),"<== before \n")
  bests,rests = all:bestRest()
  left      = all:clone(bests)
  right     = all:clone(rests)
  h         = {left=#bests, right=#rests}
  rules     =  RULE.fromBins(left:bins(right),all,h,bests,rests)
  end

function go.rules1() _rules("../etc/data/auto93.csv") end
function go.rules2() _rules("../etc/data/china.csv") end
function go.rules3() _rules("../etc/data/nasa93dem.csv") end
function go.rules4() _rules("../etc/data/pom.csv") end
function go.rules5() _rules("../etc/data/coc10000.csv") end
function go.rules6() _rules("../etc/data/auto2.csv") end


local function _dist(file,  i,all)
  local any= _.any
  i= EGS():adds(file)
  local yes=true
  all=NUM()
  for j=1,1000 do 
    if (j % 50)==0 then io.write(".") end
    local a,b,c = any(i.rows), any(i.rows), any(i.rows)
    local aa = i:dist(a,a)
    local ba = i:dist(b,a)
    local ab = i:dist(a,b)
    local bc = i:dist(b,c)
    local ac = i:dist(a,c)
    all:adds{aa,ba,ab,bc,ac}
    yes = yes and aa==0 and ab == ba and ab+bc >= ac
    yes = yes and aa>=0 and aa<=1 and ba>=0 and ba<=1 and ab>=0 and ab<=1 and
                  bc>=0 and bc <=1 and ac >= 0 and ac <= 1 end
  oo(rnds(all:all()))
  ok(yes, "dist") end 

function go.dist1() _dist(the.file) end
function go.dist2() _dist("../etc/data/diabetes.csv") end

function no.half(  i)
  the.file = "../etc/data/diabetes.csv"
  i = egs.Init(the.file) 
  local lefts,rights,left,right,border,c= cluster.half(i)
  print("rows",#i.rows)
  ok(384 == #lefts.rows,  "left")
  ok(384 == #rights.rows, "rights") end

function no.cluster(  i)
  the.file = "../etc/data/diabetes.csv"
  i = egs.Init(the.file) 
  cluster.show(cluster.new(i)) end

function go.abcd()
  local t={}
  for _ = 1,6 do push(t,{want="yes",got="yes"}) end
  for _ = 1,2 do push(t,{want="no",got="no"}) end
  for _ = 1,6 do push(t,{want="maybe",got="maybe"}) end
  for _ = 1,1 do push(t,{want="maybe", got="no"}) end
  ABCD():adds(t,true) end 

local function qq(i,q) 
  print(q[1], fmt("%15s = %-8s best= %s/%s rest= %s/%s",
                  i.cols[q[2]].name, q[3],q[4],q[5],q[6],q[7])) end

local function gonb1(file) 
  local i = require"learn101"(file)
  local _, out = i:score()
  local cnt={}
  for _,one in pairs(out) do local k=one.got..","..one.want; cnt[k] = 1+ (cnt[k] or 0) end
  for k,n in pairs(cnt) do print(n,o(k)) end 
  ABCD():adds(i.log,true) end

function go.nb1a() gonb1(the.file) end 
function go.nb1b() gonb1("../etc/data/diabetes.csv") end 

function go.nb2() 
  the.file = "../etc/data/diabetes.csv" 
  the.goal = "positive"
  local i = require("learn201")(the.file); 
  ABCD():adds(i.log,true) end 

function no.nb2a() 
  the.file = "../etc/data/diabetes.csv" 
  the.goal = "positive"
  for _,bins in pairs{2,5,9} do
    the.bins = bins
    local i = require("learn201")(the.file); 
    ABCD()(i.log,true) end end

function no.nb3() 
  the.file = "../etc/data/diabetes.csv" 
  the.goal = "positive"
  the.bins = 16
  local i = nb3(the.file); 
  abcd(i.log,true)
  local acc, out = score(i);  map(out,function(q) qq(i,q) end) end

return go
