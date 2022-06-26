local all=require"all"
local obj=all.obj

local Tree = obj("TREE", function(i,listOfRows)
  local total,rows,is = 0,{},{}
  for label,rows1 in pairs(listOfRows) do
    total = total + #rows
    for _,row in pairs(rows1) do
      rows[1+#rows]=row
      is[row.id]=label end end
  local function mapSortedBins(j)
    local function bins(xcol)
      return BIN.BINS(j.rows, xcol, SYM, function(row) return is[row.id] end) end
    local function down(bin)
      local new = j:clone(bin:holds(j.rows))
      if #new.rows<#j.rows then
        new.gaurd = bin
       return mapSortedBins(new) end end
    if #j.rows >= 2*small(total) then
      j.kids = map(sort(map(j.cols.x, bins),lt"div")[1].bins, down) end
    return j end
  return mapSortedBins(i:clone(rows)) end)
   
function Tree.show(i,lvl)
  lvl = lvl or 0
  local gaurd = i.gaurd and i.gaurd:show()
  print(fmt("%-40s", o(Data.mids(i,2))), ("| "):rep(lvl) .. (gaurd or ""))
  for _,kid in pairs(i.kids or {}) do Data.show(kid, 1+lvl) end end

