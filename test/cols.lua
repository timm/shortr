-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
require "lib"

local ok=require("ok")
local Cols=require("cols")

ok{less=function(   c)
  c= Cols.new()
  Cols.add(c,{"<age", "?id", "name", "$salary", "!job"})
  o(c.syms)
end}

