-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local o = require("lib").o
local csv = require("csv")

for line in csv("../data/weather2.csv") do
  o(line,{pre=">",sep="::"})
end
