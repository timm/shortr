-- SHORTR.lua (c) 2022, Tim Menzies <timm@ieee.org>, BSD2 license.
-- Semi-supervised multi-objective optimization XAI. From N
-- items, find and explain the best ones, using just log(N) 
-- evals.  All in a few hundreds lines of LUA.

--                                           ___                         
--                                          /\_ \                        
--   ____    ___     ___ ___       __       \//\ \     __  __     __     
--  /',__\  / __`\ /' __` __`\   /'__`\       \ \ \   /\ \/\ \  /'__`\   
-- /\__, `\/\ \L\ \/\ \/\ \/\ \ /\  __/  __    \_\ \_ \ \ \_\ \/\ \L\.\_ 
-- \/\____/\ \____/\ \_\ \_\ \_\\ \____\/\_\   /\____\ \ \____/\ \__/.\_\
--  \/___/  \/___/  \/_/\/_/\/_/ \/____/\/_/   \/____/  \/___/  \/__/\/_/
                                                                      
local obj = require("lib").obj
local the = require("the")

local Some=obj"Some"
function Some.NEW(at, txt)
  return {n=0, kept={}, i.max=at=at or 0, txt= txt or ""} end

function Some.add(i,x)
  if x == "?" then return x else
    if     #i.kept < i.max then i.ok=false;push(i.kept,v) 
    elseif R() < i.nums/i.n then i.ok=false;i.kept[R(#i.kept)]=v end end 
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
