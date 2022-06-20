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

-- lint() :nil -- report rogue globals
local b4={}; for x,_ in pairs(_ENV) do b4[x]=x end 
function lib.lint() --> ()  
  for x,v in pairs(_ENV) do if not b4[x] then print("?",x,type(v)) end end end

-- big             :num -- large number
-- fmt(:str, :str) :str -- emulate printf
-- R( max:?num=1)  :num -- return random number 0..max
lib.big = math.huge
lib.fmt = string.format
lib.R   = math.random



