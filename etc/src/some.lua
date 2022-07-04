-- ## Keep some nums
-- Given a finite buffer of some small size `max`, then after reading 
-- a very large set of `n` numbers, we should only be keeping `max/n` of those nums.

-- Requires :
local all=require"all"
local obj,push,R,sort,the= all.obj, all.push, all.R, all.sort, all.the

-- SOME(max:?int) :SOME --> collect, at most, `max` numbers.
local SOME = obj("SOME", function(i,max) 
  i.kept, i.ok, i.max, i.n = {}, true, max, 0  end)

-- add(i:SOME: x:num) --> `n` times,update `i`.
-- Helper function for NUM. If full then at odds `i.some/i.x`, keep `x`
-- (replacing some older item, at random).
function SOME.add(i,x)
  if x ~= "?" then 
    i.n = i.n + 1
    if #i.kept < i.max     then i.ok=false; push(i.kept,x) 
    elseif R() < i.max/i.n then i.ok=false; i.kept[R(#i.kept)]=x end end end 

-- has(i:SOME):tab --> Ensure contents are sorted. Return those contents.
function SOME.has(i)
  i.kept = i.ok and i.kept or sort(i.kept); i.ok=true; return i.kept ; end

return SOME
