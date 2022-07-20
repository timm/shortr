local l=require"lib"
local x=require"xplan"

----  main(tab,tab) -- Runs some (or all) of the demos. Return number of failures.
-- Resets `the` and the random number seed before each demo. 
function l.main(the,go,help) 
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

local go,no= {},{}

l.main(x.the, go, x.help)
