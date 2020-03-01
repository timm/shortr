-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Columns = require("columns")
local Lib = require("lib")

c= Columns
   {names={"outlook","<temp",
           "$humid","?wind","!play"}}

for _,vs in pairs {"nums","syms"} do
  for k,v in pairs(c[vs]) do
    print(vs,k,v) end
end

for _,xy in pairs {"x","y"} do
  for k,v in pairs(c[xy]) do
    print(xy,k,Lib.oo(v)) end end
