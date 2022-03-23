local nb1  = require"learna"
local lib  = require"lib"
local bin  = require"bin"
local collect,push = lib.collect,lib.push

local function nb3(data,  log)
  local tmp, xnums = {}
  local function discretize(c,x,   col)
    if x ~= "?" then 
      col = xnums[c]
      if col then
        for _,one in pairs(col.bins) do 
          if one.lo <= x and x < one.hi then return one.id end end end end 
    return x end

  local function xnum(c,name) 

    if ako.xnum(name) then return {name=name, xys={},bins={}} end end

  local function train(c,x,row) 
    if xnums[c] and x ~= "?" then push(xnums[c].xys, {x=x,y= row[#row]}) end end

  for row in items(data) do
    push(tmp,row)
    if   xnums then collect(row, function(c,x) return train(c,x,row) end) 
    else xnums = collect(row,xnum) end end
  for where,col in pairs(xnums) do 
    col.bins = bin.Xys(col.xys,where); print(col.name,#col.bins) end
  for j=2,#tmp do tmp[j] = collect(tmp[j], discretize) end
  return nb1(tmp) end

return nb3
