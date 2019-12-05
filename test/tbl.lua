-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path

local within=require("lib").within
local Lib=require("lib")
oo,same = Lib.oo, Lib.same
local ok=require("ok")
local Tbl=require("tbl")
local Row=require("row")

ok{tbl=function(      t,n)
  t = Tbl.new{file="../data/weather.csv"}
  assert(#t.rows==14)
  assert(t.cols.y.klass.mode=="yes")
  n = t.cols.x.nums[1]
  assert(within(6.57, n.sd, 6.58))
  assert(within(73.57, n.mu, 73.58))
end}

ok{near=function(   t,a) 
  t = Tbl.new{file="../data/weather1.csv"}
  for _,row in pairs(t.rows) do
    a=Row.neighbors(row,t)
    print("")
    print(oo(row.cells))
    print(oo(a[2][2].cells),  a[2][1])
    print(oo(a[#a][2].cells), a[#a][1]) end
end}
