
-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Ranges = require("ranges")
local Sym = require("sym")

local function trio(a)
  if(a) < 0.6 then return 'a' end
  if(a) < 0.9 then return 'b' end
  return 'c'
end

local a,n={},100
for i=1,n do
	a[#a+1] = {i/n, trio(i/n) }
end

r = Ranges {lst=a,fy=Sym}
r:make()
print(r.cuts)
