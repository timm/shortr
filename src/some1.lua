-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

-- Implements _reservoir_ sampling; i.e. keep a random sample of a stream
-- of items. 
--
-- This is a very useful when streaming over a large data space.

local Column = require("column")
local Lib      = require("lib")
local THE    = require("the")
local Some   = {is="Some"}

local r,binChop = Lib.r, Lib.binChop

function Some.all(lst, out)
  out = out or Some.new()
  for _,one in pairs(lst) do Some.add(out,one) end
  return out
end

function Some.new(t)
  local i = Column.new(t)
  i.me    = Some
  i._has   = {}
  i.divs  = nis
  i.sorted= false
  t = t or {}
  i.most  = t.most or THE.some.most or 256
  return i
end

-- Given a maximum reservoir size  then keep everything
-- up until `i.most`. 
-- Then, after seeing
-- `i.n` items, keep the next item at probability of `i.most/i.n`. 
-- And by "keeping", we mean "replace anything at random with the new item".

function Some.has(i)
  if not i.sorted then table.sort(i._has); i.sorted=true end
  return i._has
end

function Some.ptile(i,x)
  pos = binChop(Some.has(i),x)
  return pos/#i._has
end

function Some.add(i,x) 
  if x == "?" then return x end
  x = i.key(x)
  i.n  = i.n + 1
  if #i._has < i.most then 
    i.divs = nil
    i._has[#i._has+1] = x 
    i.sorted= false
  elseif r() < i.most/i.n then
    Some.has(i)[ binChop(i._has,x) ] = x end
end

-- Also supported is a hook into an unsupervised discretization function
-- that divides the kept numbers in a manner that minimizing the standard
-- deviation of the divided bins.


-- ----------
-- And finally...

return Some
