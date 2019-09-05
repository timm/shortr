#!/usr/bin/env lua
-- vim: ts=2 sw=2 sts=2  et :

require "lib"
local Abcd= require "Abcd"

ok{go=function( a,y,n,m)
  a= Abcd:new()
  y="yes"
  n="no"
  m="maybe"
  for i = 1,6 do a:add(y,y) end
  for i = 1,2 do a:add(n,n) end
  for i = 1,5 do a:add(m,m) end
  a:add(m,n) 
  print(a:show(a)[n])
end}
