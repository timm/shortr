-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 
-- [home](index.html) :: [about](about.html) :: [github](http://github.com/timm/lua) :: [discuss](http://github.com/timm/lua/issues) :: [&copy; 2020](https://github.com/timm/lua/blob/master/LICENSE.md) by [timm](http://menzies.us)


local Object = {is="Object"}

local n= 0
local function id() n=n+1; return n end

function Object.new()
  local i = {} 
  i.id, i.me = id(), Object
  return i
end

return Object
