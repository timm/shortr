local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
package.path = '../src/?.lua;' .. package.path
local o = require("tricks").o
local ABCD=require"ABCD"

local a=ABCD()
for j=1,6 do a:add("yes","yes") end
for j=1,2 do a:add("no","no") end
for j=1,5 do a:add("maybe","maybe") end
for j=1,1 do a:add("maybe","no") end

a:show()
