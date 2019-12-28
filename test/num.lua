-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local l=require("lib")
local r=require("rand").r
local ok=require("ok")
local Num=require("num")
local r,within,o = l.r, l.within, l.o

ok{basic=function()
  local s1,s2 = Num.new(), Num.new()
  for _,v in pairs{9,2,5,4,12,7,8,11,9,3,
                   7,4,12,5,4,10,9,6,9,4} do
      Num.add(s1,v) 
      Num.add(s2,v/2) end
  assert(within(3.06,s1.sd,3.07))
  assert(0.0 == Num.norm(s1,2))
  assert(within(2.29, Num.xpect(s1,s2), 2.3))
end}

ok{addsub=function(m,step)
  m, step = m or 100, step or 5
  local num = Num.new()
  local nums, b4 = {}, {}
  for n=1,m do nums[n] = r()^3 end
  for n=1,m do
    Num.add(num, nums[n]) 
    if (n % step) == 0 then 
      b4[n] = {var=Num.var(num), mid=Num.mid(num)} end end
  print("")
  for n=m,1,-1 do
    if b4[n] then
      print(n)
      local old,now = b4[n].var, Num.var(num)
      assert(within(.999, now/old, 1.001))
      local old,now = b4[n].mid, Num.mid(num)
      assert(within(.999, now/old, 1.001))
    end 
    Num.sub(num, nums[n])
  end 
end}
