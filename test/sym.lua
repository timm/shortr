-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local within = require("lib").within

local ok=require("ok")
local Sym=require("sym")

ok{sd=function( str,s)
  local s = Sym.new()
  str="aaaabbc"
  str:gsub(".", function(c) Sym.add(s,c) end)
  assert(within(1.37, Sym.ent(s), 1.38))
end}
