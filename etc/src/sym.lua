-----
-- Summarize a stream of numbers.
local _=require"about"
local obj,push,the = _.obj,_.push,_.the

--> SYM(at:?int, txt:?str) :SYM -> Summarize a stream of non-numerics.
local SYM = obj("SYM", function(i,at,txt)
  i.at, i.txt, i.n, i.kept =  at or 0, txt or "", 0, {} end)

--> add(i:SYM: x:sum, n:?int=1) -> Add `n` count to `i.kept[n]` .
function SYM.add(i,x,n)
  if x ~= "?" then 
    i.kept[x] = (n or 1) + (i.kept[x] or 0) end end

--> clone(i:(SYM|NUM)) :(SYM|NUM) -> Return a class of the same structure.
function SYM.clone(i) return SYM(i.at, i.txt) end

function SYM.like(i,x,prior)
   return ((i.kept[x] or 0)+the.m*prior) / (i.n+the.m) end

--> mid(i:SYM),p:?float=.5):tab -> Return a columns' `mid`ddle
-- (central tendency), rounded to `p` places.
function SYM.mid(i,p)
  local max,mode=-1,nil
  for x,n in pairs(i.kept) do if n > most then most,mode = n,x end end
  return rnd(mode, p or 2) end

--> div(i:SYM,p:?float=.5):tab -> Return `div`ersity of a column
-- (its tendency _not_ to be a its central tendency).
function SYM.div(i,p)
  local ent,log=0,function(x) return math.log(x,2) end
  for x,n in pairs(i.kept) do if n > 0 then ent=ent - n/i.n*log(n/i.n) end end
  return rnd(ent,p or 2) end
 
return SYM
