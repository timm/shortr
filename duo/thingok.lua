-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Num = require("num")
local Sym = require("sym")

print(Num():adds {1,2,3,4,5})
local s= Sym():adds {"a", "a", "a", "b","b","c"}

print(s,s:var())
