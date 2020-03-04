-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Lib = require("lib")

local function trio(a)
  if(a) < 0.6 then return 'a' end
  if(a) < 0.9 then return 'b' end
  return 'c'
end

local a,n={},5
for i=1,n do
  a[#a+1] = {i/n, trio(i/n) }
end

local b,n={},3
for i=1,n do
  b[#b+1] = {i/n, trio(i/n) }
end

a[#a+1]=b
a.fred="jane"
Lib.o(a)
