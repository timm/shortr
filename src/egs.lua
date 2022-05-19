-- egs.lua : example usage of the ego.lua
-- (c) 2022 Tim Menzies.  Usage of the works is permitted provided that this
-- instrument is retained with the works, so that any entity that uses the works
-- is notified of this instrument. DISCLAIMER:THE WORKS ARE WITHOUT WARRANTY.  
local etc=require"etc"
local ego= require"ego"

local any,map,o,oo,push,sort = etc.any, etc.map, etc.o, etc.oo, etc.push, etc.sort
local csv,splice = etc.csv, etc.splice
local SOME,NUM,SYM,EGS,ROWS = ego.SOME,ego.NUM,ego.SYM,ego.EGS, ego.ROWS

--------------------------------------------------------------------------------
local the =  etc.cli(ego.the,ego.help)
local go,no={},{} -- place to store enabled and disabled tests

function go.the() 
  if the.loud then oo(the) end ; return type(the.seed) == "number" end

function go.map( t) 
  t= map({10,20,30},function(x) return x*10 end)
  if the.loud then oo(t) end
  return 100==t[1] end

function go.splice(   t) 
  t=splice( { 10,220,230,240,250,260,270,280,290,110,320,330,340,350,360,370,380,390, 
             210,420,430,440,450,460,470,480,490,210,520,530,540,550,560,570,580,590},
             10,36,4) 
  if the.loud then oo(t) end
  return t[#t]==570 end

function go.csv(    n) 
  n=0; for t in csv("../etc/data/auto93.csv") do 
    if n>100 and type(t[1]) ~= "number" then return "bad type" end
    n=n+#t end
  if the.loud then print(n,"cells") end
  return n==3192 end

local function ish(x,y,e) return math.abs((x-y)/x) < e end

function go.merge( s1,s2,s3,a)
  a={"a","a","a","a","b","b","c"}
  s1=SYM(); for _,x in pairs(a) do s1:add(x) end
  s2=SYM(); for _,x in pairs(a) do s2:add(x) end
  s3=s1:clone():inject(s1,s2)
  if the.loud then print(o(s3.has), "entropy:",s3:div()) end
  return s3.has.a==8 and s3.has.c==2 end

function go.some( s)
  the.keep=128
  s=SOME()
  for i=1,100000 do s:add(any{1,2,3,4,5,6,7,8,9,10}) end
  if the.loud then oo(s:all()) end
  return #s.has==the.keep end

function go.sym( s)
  s=SYM()
  for i=1,1000 do s:add(any{"a","a","a","a","b","b","c"}) end
  if the.loud then print(o(s.has), "entropy:",s:div()) end
  return ish(s.has.a/s.has.b, 2,.1) and ish(s.has.b/s.has.c, 2,.1) end

function go.num(  n)
  n=NUM()
  for _,x in pairs{85,86,100,76,81,93,84,99,71,69,93,85,81,87,89} do n:add(x) end 
  if the.loud then print(o(n),"standard deviation:", n:div()) end
  return ish(n:div(),9,0.001)  end

function go.egs(    r) 
  return 5140 == ROWS("../etc/data/auto93.csv").y[1].hi end

function go.xx(    r) 
  ROWS("../etc/data/auto93.csv"):xx() end
--------------------------------------------------------------------------------

local function demos(    fails,names,defaults,status)
  fails=0     -- this code will return number of failures
  names, defaults = {},{}
  for k,f in pairs(go) do if type(f)=="function" then push(names,k) end end 
  for k,v in pairs(the) do defaults[k]=v end
  if go[the.go] then names={the.go} end
  for _,one in pairs(sort(names))  do         -- for all we want to do
    for k,v in pairs(defaults) do the[k]=v end -- set settings to defaults
    math.randomseed(the.seed or 10019)         -- reset random number seed
    io.stderr:write(".")
    status = go[one]()                         -- run demo
    if status ~= true then
      print("-- Error",one,status) 
      fails = fails + 1 end end                -- update fails
  for k,v in pairs(_ENV) do if not etc.b4[k] then print("?",k,type(v)) end end
  return fails end                             -- return total failure count

os.exit(demos())
