-- ## Summarize symbols

local all = require"all"
local chat,obj,push,the = all.chat, all.obj, all.push, all.the
-- SYM(at:?int, txt:?str) :SYM --> Summarize a stream of non-numerics.
local SYM = obj("SYM", function(i,at,txt)
  i.at, i.txt, i.n, i.kept =  at or 0, txt or "", 0, {} end)

-- add(i:SYM: x:any, n:?int=1) --> Add `n` count to `i.kept[n]` .
function SYM.add(i,x,n)
  if x ~= "?" then 
    n = n or 1
    i.n = i.n+n
    i.kept[x] = n  + (i.kept[x] or 0) end end

-- bin(i:SYM: x:any) --> return `x` mapped to a finite range (just return x)
function SYM.bin(i,x) return x end

-- clone(i:SYM) :SYM --> Return a class of the same structure.
function SYM.clone(i) return SYM(i.at, i.txt) end

-- like(i:SYN,x:any,prior:num):num --> return how much `x` might belong to `i`.
function SYM.like(i,x,prior)
   return ((i.kept[x] or 0)+the.m*prior) / (i.n+the.m) end

-- merge(i:SYM,j:SYM):SYM --> combine two symbols
function SYM.merge(i,j,     k)
  k = i:clone()
  for _,kept in pairs{i.kept, j.kept} do
    for x,n in pairs(kept) do k:add(x,n) end end
  return k end

-- merge(i:SYM,t:tab):tab --> merge a list of bins (for symbolic y-values)
function SYM.merges(i,t,...) return t end
 
-- mid(i:SYM):tab --> Return a columns' `mid`ddle (central tendency).
function SYM.mid(i,p)
  local max,mode=-1,nil
  for x,n in pairs(i.kept) do if n > most then most,mode = n,x end end
  return mode end

-- div(i:SYM):tab --> Return `div`ersity of a column (its tendency _not_ to be a its central tendency).
function SYM.div(i,p)
  local ent, fun = 0, function(p) return -p*math.log(p,2) end
  for x,n in pairs(i.kept) do if n > 0 then ent=ent + fun(n/i.n) end end
  return ent end

-- That's all folks.
return SYM
