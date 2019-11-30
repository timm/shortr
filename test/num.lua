-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

require "lib"

local ok=require("ok")
local Num=require("num")

ok{less=function(   n)
  n = Num.new{txt = "<newThing" }
  assert(n.w == -1,"badly weighted")
end}

ok{sd=function()
  local s1,s2 = Num.new(), Num.new()
  for _,v in pairs{9,2,5,4,12,7,8,11,9,3,
                   7,4,12,5,4,10,9,6,9,4} do
      Num.add(s1,v) 
      Num.add(s2,v/2) end
  assert(within(3.06,s1.sd,3.07))
  print(s1.sd, s2.sd)
  print(Num.norm(s1,2))
  print(Num.xpect(s1,s2))
end}
