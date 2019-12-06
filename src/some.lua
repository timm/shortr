-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

-- <img align=right width=150 src="https://github.com/timm/lua/raw/master/etc/img/reservoir.png">
-- Implements _reservoir_ sampling; i.e. keep a random sample of a stream
-- of items. 
--
-- This is a very useful when streaming over a large data space.

local Column = require("column")
local r      = require("lib").r
local divs   = require("divs")
local THE    = require("the")
local Some   = {is="Some"}

function Some.new(t)
  local i = Column.new(t)
  i.me    = Some
  i.has   = {}
  i.divs  = nil
  t = t or {}
  i.most  = t.most or THE.some.most or 256
  return i
end

-- Given a maximum reservoir size  then keep everything
-- up until `i.most`. 
-- Then, after seeing
-- `i.n` items, keep the next item at probability of `i.most/i.n`. 
-- And by "keeping", we mean "replace anything at random with the new item".

function Some.add(i,x) 
  if x == "?" then return x end
  x = i.key(x)
  i.n  = i.n + 1
  if #i.has < i.most then 
    i.divs = nil
    i.has[#i.has+1] = x 
  elseif r() < i.most/i.n then
    i.divs = nil
    i.has[ math.floor(#i.has*r()) + 1 ] = x end
end

-- Also supported is a hook into an unsupervised discretization function
-- that divides the kept numbers in a manner that minimizing the standard
-- deviation of the divided bins.

function Some.divs(i) 
  i.divs = i.divs and i.divs or divs(i.has) 
  return i.divs
end

-- ----------
-- And finally...

return Some
