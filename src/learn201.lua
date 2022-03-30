local R=require
local the,_, ako, NB = R"the",R"lib",R"ako", R"learn101"
local push,items,collect = _.push, _.items, _.collect

return function(data)
  local tmp,xnums = {}
  local function go(c,x,    col)
    if x ~= "?" then 
      col = xnums[c]
      if col then x=(x - col.lo) // ((col.hi - col.lo+1E-32) / the.bins) end end
    return x end

  local function xnum(c,name) 
    if ako.xnum(name) then return {lo=1E32, hi=-1E32} end end

  local function train(c,x,    col) 
    col = xnums[c]
    if col and x ~= "?" then 
       col.hi = math.max(x, col.hi)
       col.lo = math.min(x, col.lo) end 
    return x end

  print("dat",data)
  for row in items(data) do 
    push(tmp, row) 
    if   xnums then collect(row, train) 
    else xnums = collect(row,xnum)  end end
  for j=2,#tmp do tmp[j] = collect(tmp[j], go) end
  return NB(tmp) end
