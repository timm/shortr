-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
require "lib"
local ok = require("ok")
local csv1 = require("csv1")

for line in csv1("../data/weather.csv") do
  o(line)
end
for line in csv1("../data/weather.csv") do
  o(line)
end

