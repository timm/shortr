-- ## class SYM: summarize symbols

local all = require"all"
local chat,obj,push,the = all.chat, all.obj, all.push, all.the
--> SYM(at:?int, txt:?str) :SYM -> Summarize a stream of non-numerics.
local SYM = obj("SYM", function(i,at,txt)
  i.at, i.txt, i.n, i.kept =  at or 0, txt or "", 0, {} end)

--> add(i:SYM: x:sum, n:?int=1) -> Add `n` count to `i.kept[n]` .
function SYM.add(i,x,n)
  if x ~= "?" then 
    i.n = i.n+1
    i.kept[x] = (n or 1) + (i.kept[x] or 0) end end

--> clone(i:SYM) :SYM -> Return a class of the same structure.
function SYM.clone(i) return SYM(i.at, i.txt) end

--> like(i:SYN,x:any,prior:num):num -> return how much `x` might belong to `i`.
function SYM.like(i,x,prior)
   chat{i.kept[x],the.m,prior,i.n, the.m}
   return ((i.kept[x] or 0)+the.m*prior) / (i.n+the.m) end

--> mid(i:SYM):tab -> Return a columns' `mid`ddle (central tendency).
function SYM.mid(i,p)
  local max,mode=-1,nil
  for x,n in pairs(i.kept) do if n > most then most,mode = n,x end end
  return mode end

--> div(i:SYM):tab -> Return `div`ersity of a column
-- (its tendency _not_ to be a its central tendency).
function SYM.div(i,p)
  local ent, log = 0, function(x) return math.log(x,2) end
  for x,n in pairs(i.kept) do if n > 0 then ent=ent - n/i.n*log(n/i.n) end end
  return ent end
 
return SYM
