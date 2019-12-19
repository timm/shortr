-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local heap=require("heap")
local ok=require("ok")


o{heap= function( h) 
  math.randomseed(1)
  h=heap{cmp = function(a,b)  return a.age < b.age end}
  for i = 10^2,1,-1 do h:push({age=math.random()}) end
  for i=1,h:length() do print(i,h:peek(i).age) end
end}

