# Mr Bill


```lua
local Sym={}

-- .NEW(  at:num,  txt:str)  :Sym
function Sym.NEW(at,txt) 
  return {_is=Sym,at=at or 0,txt=txt or {},n=0,kept={},
         w=(txt or ""):find"-$" and -1 or 1} end

-- .add( i:Sym, x:any, inc :?num ) :Sym
function Sym.add(i,x,inc) 
  if x=="?" then return else
    i.n = i.n + 1
    i.kept[x] = inc + (i.kept[x] or 0) 
    if i.kept[x] > i.most then i.most,i.mode = i.kept[x],x end end end

-- .mid( i :Sym ) :num
function Sym.mid(i) return i.mode end

-- .div( i :Sym ) :num
function Sym.div(i,  e)
  local log=function(x) return math.log(x,2) end
  e=0; for _,v in pairs(i.kept) do if v>0 then e=e-v/i.n*log(v/i.n) end end
  return e end

return Sym
```
