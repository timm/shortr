-- egs.lua : example usage of the ego.lua
-- (c) 2022 Tim Menzies.  Usage of the works is permitted provided that this
-- instrument is retained with the works, so that any entity that uses the works
-- is notified of this instrument. DISCLAIMER:THE WORKS ARE WITHOUT WARRANTY.  

--------------------------------------------------------------------------------
local etc=require"etc"
local ego= require"ego"
local oo,push,sort = etc.oo, etc.push, etc.sort
local the = ego.the
local EGS = ego.EGS
local go,no={},{} -- place to store enabled and disabled tests

function no.load(x)
  for i=1,5 do oo(x.rows[i]) end; print""
  for i=#x.rows-5,#x.rows do oo(x.rows[i]) end
end

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
os.exit(demos())
