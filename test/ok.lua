-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local ok = require("ok")

roguevar=22

ok{notok = function() assert(1~=1,"not eq") end}
ok{ok1 = function() assert(1==1,"not equal") end}

