--vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

local Thing = require("thing")
local div   = require("divs")
local Some  = {klass="Some"}

function Some.new(t)
  i      = Thing.new(t)
  i.me   = Some
  i.has  = {}
  i.divs = nil
  i.most = t.most or THE.some.most
  return i
end

function Some.div(i) 
  i.divs = i.divs and i.divs or divs(i.has) 
  return i.divs
end

function Some.add(i,x) 
  if x == "?" then return x end
  x = i.key(x)
  i.n  = i.n + 1
  i.divs = nil
  if #i.has < i.most then 
    i.has[#i.has+1] = x 
  elseif r() < i.most/i.n then
    i.has[ math.floor(#i.has*r()) + 1 ] = x end
end

return Some
