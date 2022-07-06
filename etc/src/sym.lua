-- ## Class SYM
-- Summarize symbols

-- **RESPONSIBILITIES** : 
-- - Same responsibilities as [NUM](num.md) (but for symbols)
-- - Create a duplicate structure (see `clone`)
-- - Incremental summarization (see `add`)
-- - Discretization (see `bin, merge, merges`)
-- - Distance calcs (see `dist`)
-- - Likelihood calcs (see `like`)
-- - Knows central tendency and diversity (see `mids, divs`)
-- ------------------------------------------------------------
local all = require"all"
local chat,obj,push,the = all.chat, all.obj, all.push, all.the

-- SYM(at:?int, txt:?str) :SYM --> #CONSTRUCTOR. Summarize a stream of non-numerics.
local SYM = obj("SYM", function(i,at,txt)
  i.at   = at or 0   -- :num -- column position 
  i.txt  = txt or "" -- :str -- column name 
  i.n    = 0         -- :num -- items seen so far
  i.kept = {}        -- :tab -- counts of symbols seen so far
  end)

-- add(i:SYM: x:any, n:?int=1) --> #UPDATE. Add `n` count to `i.kept[n]` .
function SYM.add(i,x,n)
  if x ~= "?" then 
    n = n or 1
    i.n = i.n+n
    i.kept[x] = n  + (i.kept[x] or 0) end end

-- bin(i:SYM: x:any) --> #DISCRETIZE. Return `x` mapped to a finite range (just return x)
function SYM.bin(i,x) return x end

-- clone(i:SYM) :SYM --> #cCREATE. Return a class of the same structure.
function SYM.clone(i) return SYM(i.at, i.txt) end

-- dist(i:SYM, x:any,y:any): num --> #DISTANCE. Return distance 0..1 between `x,y`. Assume max distance for missing values.
function SYM.dist(i,x,y)
  return  (x=="?" or y=="?")  and 1 or x==y and 0 or 1 end

-- like(i:SYN,x:any,prior:num):num --> #LIKE. Return how much `x` might belong to `i`.
function SYM.like(i,x,prior)
   return ((i.kept[x] or 0)+the.m*prior) / (i.n+the.m) end

-- merge(i:SYM,j:SYM):SYM --> #DISCRETIZ. Combine two symbols
function SYM.merge(i,j,     k)
  k = i:clone()
  for _,kept in pairs{i.kept, j.kept} do
    for x,n in pairs(kept) do k:add(x,n) end end
  return k end

-- merge(i:SYM,t:tab):tab --> #DISCRETIZE. merge a list of bins (for symbolic y-values)
function SYM.merges(i,t,...) return t end
 
-- mid(i:SYM):tab --> #report: Return a columns' `mid`ddle (central tendency).
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
