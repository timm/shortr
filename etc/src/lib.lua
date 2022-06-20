-- SHORTR.lua (c) 2022, Tim Menzies <timm@ieee.org>, BSD2 license.
-- Semi-supervised multi-objective optimization XAI. From N
-- items, find and explain the best ones, using just log(N) 
-- evals.  All in a few hundreds lines of LUA.

--  ___              __             ___                         
-- /\_ \      __    /\ \           /\_ \                        
-- \//\ \    /\_\   \ \ \____      \//\ \     __  __     __     
--   \ \ \   \/\ \   \ \ '__`\       \ \ \   /\ \/\ \  /'__`\   
--    \_\ \_  \ \ \   \ \ \L\ \ __    \_\ \_ \ \ \_\ \/\ \L\.\_ 
--    /\____\  \ \_\   \ \_,__//\_\   /\____\ \ \____/\ \__/.\_\
--    \/____/   \/_/    \/___/ \/_/   \/____/  \/___/  \/__/\/_/

local lib = {}

--> lint() :nil -> report rogue globals
local b4={}; for x,_ in pairs(_ENV) do b4[x]=x end 
function lib.lint() --> ()  
  for x,v in pairs(_ENV) do if not b4[x] then print("?",x,type(v)) end end end

--> big             :num -> large number
--> fmt(:str, :str) :str -> emulate printf
--> R( max:?num=1)  :num -> return random number 0..max
lib.big = math.huge
lib.fmt = string.format
lib.R   = math.random


function obj(name,    t,new)
  function new(kl,...) 
     local x=setmetatable({id=id()},kl); kl.new(x,...); return x end 
       t = {__tostring=o, is=name or ""}; t.__index=t
         return setmetatable(t, {__call=new}) end

--> thing(x :str) :any -> coerce x to some LUA type
function thing(x)
  if type(x)~="string" then return x end
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x end

--> help(x :str) :tab -> for lines with "--", pull keys+defaults. 
-- Look for updates for "key" on command-line. 
function help(str)
  local t = {}
  str:gsub("\n  ([-][-]([^%s]+))[%s]+(-[^%s]+)[^\n]*%s([^%s]+)",
    function(f1,k,f2,x)
      for n,flag in ipairs(arg) do if flag==f1 or flag==f2 then
        x = x=="false" and"true" or x=="true" and"false" or arg[n+1] end end 
        t[k] = thing(x) end) 
   if t.help then 
     os.exit(print(str:gsub("[%u][%u%d]+","\27[1;32m%1\27[0m"),"")) end
  return t end

--> csv(src :str, fun :function) :nil -> for file lines, split on "," pass to fun
function csv(file, fun,    line,t)
  file  = io.input(file)
  line = io.read()
  while line do
    t={}; for x in line:gmatch("([^,]+)") do t[1+#t]=thing(x) end; fun(t) 
    line = io.read() end
  io.close(file) end
