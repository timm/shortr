-- SHORTR.lua (c) 2022, Tim Menzies <timm@ieee.org>, BSD2 license.
-- Semi-supervised multi-objective optimization XAI. From N
-- items, find and explain the best ones, using just log(N) 
-- evals.  All in a few hundreds lines of LUA.

--                                        ___                        
--                                       /\_ \                       
--   ____  __  __    ___ ___             \//\ \    __  __     __     
--  /',__\/\ \/\ \ /' __` __`\             \ \ \  /\ \/\ \  /'__`\   
-- /\__, `\ \ \_\ \/\ \/\ \/\ \      __     \_\ \_\ \ \_\ \/\ \L\.\_ 
-- \/\____/\/`____ \ \_\ \_\ \_\    /\_\    /\____\\ \____/\ \__/.\_\
--  \/___/  `/___/> \/_/\/_/\/_/    \/_/    \/____/ \/___/  \/__/\/_/
--             /\___/                                                
--             \/__/                                                  

local obj  = require("lib").obj

local Sym=obj"Sym"
function Sym.NEW(at, txt)
  return {n=0, kept={}, at=at or 0, txt= txt or ""} end

function Sym.add(i,x,n)
  if x ~= "?" then
    n = n or 0
    i.n = i.n + n
    i.kept[x] = n + (i.kept[x] or 0) end end

function Sym.mid(i) 
  for x,n in pairs(i.kept) do if n>most then mode,most=x,n end end
  return mode end

function Sym.div(i) 
  local e,log = 0,function(x) return math.log(x,2) end
  for x,n in pairs(i.kept) do if n>0 then e=e-n/i.n*log(n/i.n) end end
  return e end

return Sym
