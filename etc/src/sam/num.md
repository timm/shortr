```lua
local add  = require("poly").add
local Some = require"some"

local Num={}
function Num.NEW(at,txt)  
  return {_is=Num, at=at or 0, txt=txt or {}, n=0, kept=Some(),
          w=(txt or ""):find"-$" and -1 or 1} end

function Num.add(i,x,n) 
  if x~="?" then return else
    n = n or 1
    for _ = 1,n do i.n=i.n+1; i.some:add(x) end end end 

function Num.mid(i,p,   a) a=i.some:has(i); return rnd(per(a, .5),p) end
function Num.div(i,p,   a) 
  a=i.some:has();return rnd((per(a,.9)-per(a,.1))/2.56,p) end

function Num.norm(i,x)
  if x=="?" then return x else
    a=i.some:has()
    return  a[#a] - a[1] < 1E-9 and 0 or (x - a[1])/(a[#a] - a[1]) end end
```

return Num
