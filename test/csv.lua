-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
require "lib"
local csv = require("csv")

csv("../data/weather.csv", o, o)
csv("../data/weather.csv", o, o)

