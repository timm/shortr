-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 
 
local THE  = require("the")
local Lib  = require("lib")
local Some = require("some")
local Num  = require("num")
local divs2= require("divs")
local Tree = {is="Tree"}
local sprintf, has, same = Lib.sprintf, Lib.has, Lib.same

function Tree.new(tbl,my)
  local i = Object.new()
  i.me  = Tree
  i.my  = has(my){minobs=2,fy=same,ytype=Num}
  i.tbl = tbl
  i.kids= {}
  i.here= nil
  i.fx  = nil
  Tree.split(i,tbl.rows,0)
  return i
end

function Tree.split(i,lst,lvl)
  local getter, argmin, which 
  lvl = lvl or 0
  i.here = ytype.all(lst,fy)
  function getter(col) 
    return function(row) return row.cells[col.pos] end end
  function argmin()
    local lo, cuts, col = 10^32,{},nil
    for _,col1 in pairs(i.tbl.cols.indep) do
      local fx = getter(col)
      local lo1,cuts1 = col1.me.div(col1,lst,fx,i.fy,i.ytype)
      if lo1 < lo then
        cuts, col, lo  = cuts1, col1, lo1  end end
    return col,cuts
  end
  function which(row)
    for n,cut in pairs(i.cuts) do
      if i.fx(row) >= cut.lo and i.fx(row) <= cut.hi then
        return n end end
  end
  if #lst >= THE.tree.minObs then
    local col, i.cuts = argmin()
    if col then
      i.fx = getter(col)
      for _,row in pairs(lst) do
        local n   = which(row) or 1
        i.kids[n] = i.kids[n] or {kid={}, test=i.cuts[n]}
        local go = i.kids[n].kid
        kid[ #kid + 1 ] = row
      end
      for _,one in pairs(i.kids) do
        one.kid = Tree.split(i, one.kid, lvl+1) end end end 
  return i
end

function Tree.show(i,pre)
  pre = pre or ""
  printf("%s: %s", pre,i.here.n)
  if   #i.kids == 0 
  then print(i.me.mid(i.here), i.me.var(i.here))
  else print("")
  end   
  for _,one in pairs(i.kids) do 
    pre=pre .. "|   "
    Tree.show(k,pre) end
end


