local R = require
--local the,_,abcd,bin,rule        = R"the", R"lib", R"abcd",R"bin",R"rule"
local _,the,ABCD = R"lib", R"the",R"ABCD"
--local num, sym                     = R"num", R"sym"
--local ako, egs, seen, cluster      = R"ako", R"egs", R"seen", R"cluster"
--local learn101, learn201, learn301 = R"learn101", R"learn201", R"learn301"

local ish,copy,items,o,oo,powerset = _.ish,_.copy,_.items,_.o,_.oo,_.powerset
local map,fmt,rnds, rnd,push       = _.map,_.fmt,_.rnds, _.rnd,_.push
local class,Obj = _.class, _.Obj
local go,ok = _.go,_.ok

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

function go.ent()
  local a,b = _.ent{a=9,b=7}
  ok(ish(lib.ent{a=9,b=7}, .98886), "entropy")  end

function go.items()
  for  x in items{10,20,30} do oo(x) end 
  local n=0
  for x in items(the.file) do n=n+1; if n<=5 then oo(x) end end end

function go.powerset()
  for _,x in pairs(powerset{10,20,30,40,50}) do oo(x) end end
 
 function go.many( t)
  local o,many=lib.o,lib.many
  t={};for j = 1,1000 do t[#t+1] = j end
  print(900,"+", o(many(t, 10, 900)))
  print(1,100,   o(many(t, 10,   1, 100)))
  print(300,700, o(many(t, 10, 300, 700))) end 

function go.new()
  lib.dent(seen.new{"Name","Age","gender","Weight-"}) end

-- function go.clone(   i,t,best,rest, x)
--   i={rows={},cols=nil}
--   the.file = "../etc/data/auto93.csv"
--   bins=xplain(the.file) 
--   for _,row in pairs(i.rows) do
--       x=row[col].at end end

function go.egs(  i)
  i=egs.Init(the.file) 
  ok(7==i.cols.x[2].has["lt40"], "counts")
  ok(286 == #i.rows,"egs") end

function go.dist(  i)
  local any= lib.any
  i=egs.Init(the.file) 
  local yes=true
  for j=1,1000 do 
    if (j % 50)==0 then io.write(".") end
    local a,b,c = any(i.rows), any(i.rows), any(i.rows)
    local aa = cluster.dist(i,a,a)
    local ba = cluster.dist(i,b,a)
    local ab = cluster.dist(i,a,b)
    local bc = cluster.dist(i,b,c)
    local ac = cluster.dist(i,a,c)
    yes = yes and aa==0 and ab == ba and ab+bc >= ac
    yes = yes and aa>=0 and aa<=1 and ba>=0 and ba<=1 and ab>=0 and ab<=1 and
                  bc>=0 and bc <=1 and ac >= 0 and ac <= 1 end
  ok(yes, "dist") end 

function go.half(  i)
  the.file = "../etc/data/diabetes.csv"
  i = egs.Init(the.file) 
  local lefts,rights,left,right,border,c= cluster.half(i)
  print("rows",#i.rows)
  ok(384 == #lefts.rows,  "left")
  ok(384 == #rights.rows, "rights") end

function go.cluster(  i)
  the.file = "../etc/data/diabetes.csv"
  i = egs.Init(the.file) 
  cluster.show(cluster.new(i))
end

function go.abcd()
  local t={}
  for _ = 1,6 do push(t,{want="yes",got="yes"}) end
  for _ = 1,2 do push(t,{want="no",got="no"}) end
  for _ = 1,6 do push(t,{want="maybe",got="maybe"}) end
  for _ = 1,1 do push(t,{want="maybe", got="no"}) end
  abcd(t,true) end

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

function go.nb2a() 
  the.file = "../etc/data/diabetes.csv" 
  the.goal = "positive"
  for _,bins in pairs{2,5,9} do
    the.bins = bins
    local i = nb2(the.file); 
    abcd(i.log,true) end end

function go.bins(   t)
  local t,n = {},30
  for j=1,n do push(t, {x=j, y=j<.6*n and 1 or j<.8*n and 2 or 3}) end
  map(bins(t,20),oo) end

function go.nb3() 
  the.file = "../etc/data/diabetes.csv" 
  the.goal = "positive"
  the.bins = 16
  local i = nb3(the.file); 
  abcd(i.log,true)
  local acc, out = score(i);  map(out,function(q) qq(i,q) end) end

return go
