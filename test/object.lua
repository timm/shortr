-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
o=require("lib").o
local Object=require("object")

o( Object.new() )
o{aa=1, bb=2, cc={dd=22, ee=30, ff={10}}}
