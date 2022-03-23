---       . _|_ |_     _      _|
---    VV |  |  | |   (/_ VV (_|
   
local ako = require"ako"
local nb1 = require"learn101"

local function nb2(data,  log)
  local tmp,xnums = {}
  local function discretize(c,x,    col)
    if x ~= "?" then 
      col = xnums[c]
      if col then x=(x - col.lo) // ((col.hi - col.lo+1E-32) / the.bins)  end end
    return x end
  local function xnum(c,name) 
    if ako.xnum(name) then return {lo=1E32, hi=-1E32} end end
  local function train(c,x,    col) 
    col = xnums[c]
    if col and x ~= "?" then 
       col.hi = math.max(x, col.hi)
       col.lo = math.min(x, col.lo) end 
    return x end
  -- start
  for row in items(data) do 
    push(tmp, row) 
    if   xnums then collect(row, train) 
    else xnums = collect(row,xnum)  end end
  for j=2,#tmp do tmp[j] = collect(tmp[j], discretize) end
  return nb1(tmp) end

return nb2
