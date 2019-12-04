-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path

local ok=require("ok")
local Tbl=require("tbl")

ok{num=function(t)
  t = Tbl.new{file="../data/weather.csv"}
  assert(#t.rows==14)
  assert(t.cols.y.klass.mode=="yes")
  local n = t.cols.x.nums[1]
  assert(within(6.57,n.sd,6.58))
  assert(within(73.57, n.mu, 73.58))
end}
