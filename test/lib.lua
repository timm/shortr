-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
require "lib"
local ok = require("ok")

roguevar=22

ok{notok = function() assert(1~=1,"not eq") end}
ok{ok = function() assert(1==1,"not equal") end}

o{aa=1, bb=2, cc={dd=22, ee=30, ff={10}}}
