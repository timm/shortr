-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path
local lib = require("lib")
local ok = require("ok")
local r  = require("rand")

ok{rand = function(    n,a,b)
  n=10^4
  r.seed(); a={}; for i=1,n do a[#a+1]=lib.round(r.r(),3) end
  r.seed(); b={}; for i=1,n do b[#b+1]=lib.round(r.r(),3) end
  for i=1,n do assert(a[i] == b[i]) end
end}

