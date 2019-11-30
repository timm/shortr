-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 
-- [home](index.html) :: [about](about.html) :: [github](http://github.com/timm/lua) :: [discuss](http://github.com/timm/lua/issues) :: [&copy; 2020](https://github.com/timm/lua/blob/master/LICENSE.md) by [timm](http://menzies.us)


local Column = require("columns")
local div   = require("divs")
local Some  = {is="Some"}

function Some.new(t)
  local i = Column.new(t)
  i.me    = Some
  i.has   = {}
  i.divs  = nil
  i.most  = t.most or THE.some.most
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
