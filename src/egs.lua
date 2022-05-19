-- egs.lua : example usage of the ego.lua
-- (c) 2022 Tim Menzies.  Usage of the works is permitted provided that this
-- instrument is retained with the works, so that any entity that uses the works
-- is notified of this instrument. DISCLAIMER:THE WORKS ARE WITHOUT WARRANTY.  
local etc=require"etc"
local ego= require"ego"
local map,o,oo,push,sort = etc.map, etc.o, etc.oo, etc.push, etc.sort
local csv,splice = etc.csv, etc.splice
local the = ego.the
local EGS,ROWS = ego.EGS, ego.ROWS
local go,no={},{} -- place to store enabled and disabled tests

function go.the() return type(the.seed) == "number" end
function go.map() return 100==map({10,20,30},function(x) return x*10 end)[1] end
function go.splice(   t) 
  t=splice( { 10,220,230,240,250,260,270,280,290,110,320,330,340,350,360,370,380,390, 
             210,420,430,440,450,460,470,480,490,210,520,530,540,550,560,570,580,590},
             10,36,4) 
  return t[#t]==570 end

function go.csv(    n) 
  n=0; for t in csv("../etc/data/auto93.csv") do 
    if n>100 and type(t[1]) ~= "number" then return "bad type" end
    n=n+#t end
  return n==3192 end

function go.egs(    n) ROWS("../etc/data/auto93.csv")  end

--------------------------------------------------------------------------------
local function demos(    fails,names,defaults,status)
  fails=0     -- this code will return number of failures
  names, defaults = {},{}
  for k,f in pairs(go) do if type(f)=="function" then etc.push(names,k) end end 
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

the = etc.settings(ego.help)
print(type(the.keep))
os.exit(demos())
