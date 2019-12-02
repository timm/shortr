-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
require "lib"

local ok=require("ok")
local Columns=require("columns")

ok{less=function(   c)
  c= Columns.new()
  Columns.add(c,{"<age", "?id", "name", "$salary", "!job"})
  o(c.syms)
end}

