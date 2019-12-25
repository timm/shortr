-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

-- Implements _reservoir_ sampling; i.e. keep a random sample of a stream
-- of items. 
--
-- This is a very useful when streaming over a large data space.

local THE    = require("the")
local Lib    = require("lib")
local Some   = require("some1")
local Super  = {is="Super"}

local r,binChop = Lib.r, Lib.binChop

function (a, my)
  my= has(THE.divs)
  my= has(my){ ytype  = Num}
  local t= Some(a,y)

  local function prune(t) 
    local function v(z) return ytype.var(z.ys),z.ys.n end 
    local l,n1   = v(t.left)
    local r,n2   = v(t.right)
    local here,n = v(t)
    local bad    = (n1/n*l+ n2/n*r)*trivial >= here 
    t.left       = bad and nil or prune(t.left)
    t.right      = bad and nil or prune(t.right)
    return t 
  end
    
  for _,z in pairs(a) do peek(t, z) end
  return prune(t)
end
