-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local o=require("lib").o
local ok = require("ok")
local csv = require("csv")

for line in csv("../data/weather.csv") do
  o(line)
end
for line in csv("../data/weather.csv") do
  o(line)
end

