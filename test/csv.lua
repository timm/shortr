-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
require "lib"
local ok = require("ok")
local csv = require("csv")

ok{once=function() csv("../data/weather.csv", o) end}
ok{twice=function() csv("../data/weather.csv", o) end}

