
-- SHORTR.lua (c) 2022, Tim Menzies <timm@ieee.org>, BSD2 license.
-- Semi-supervised multi-objective optimization XAI. From N
-- items, find and explain the best ones, using just log(N) 
-- evals.  All in a few hundreds lines of LUA.

--         __                       __        ___                        
--        /\ \                     /\ \__    /\_ \                       
--    __  \ \ \____    ___   __  __\ \ ,_\   \//\ \    __  __     __     
--  /'__`\ \ \ '__`\  / __`\/\ \/\ \\ \ \/     \ \ \  /\ \/\ \  /'__`\   
-- /\ \L\.\_\ \ \L\ \/\ \L\ \ \ \_\ \\ \ \_  __ \_\ \_\ \ \_\ \/\ \L\.\_ 
-- \ \__/.\_\\ \_,__/\ \____/\ \____/ \ \__\/\_\/\____\\ \____/\ \__/.\_\
--  \/__/\/_/ \/___/  \/___/  \/___/   \/__/\/_/\/____/ \/___/  \/__/\/_/
          
          

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
