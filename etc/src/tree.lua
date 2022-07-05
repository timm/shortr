-- ## ROWS to tree
local all = require"all"
local chat,lt,map    = all.chat,all.lt, all.map
local small,sort,the = all.small, all.sort,all.the
local ROWS = require"rows"
local BIN = require"bin"
local SYM = require"sym"

function ROWS.tree(i, listOfRows)
  local labels, root = {}, i:clone()
  for label,rows1 in ipairs(listOfRows) do
    for _,row in pairs(rows1) do
      root:add(row)
      labels[row._id]=label end end                 -- set label
  local function y(row) return labels[row._id] end -- get label
  return root:kids(2 * small(the.Min, #root.rows), y) end

function ROWS.kids(i, stop, y)
  if #i.rows >= stop then
    local all  = map(i.cols.x, function(xcol) 
                     return BIN.BINS(i.rows,xcol,SYM,y) end) 
    local best = sort(all, lt"div")[1]
    i.kids     = map(best.bins, function (bin)
                  local new = i:clone(bin:holds(i.rows))
                  if #new.rows < #i.rows then
                    new.gaurd = bin
                    return new:kids(stop, y) end end) end
    return i end


function ROWS.branches(i,lvl)
  lvl = lvl or 0
  local gaurd = i.gaurd and i.gaurd:show()
  print(fmt("%-40s", cat(i:mids(i))), ("| "):rep(lvl) .. (gaurd or ""))
  for _,kid in pairs(i.kids or {}) do 
    kid:branches(1+lvl) end end
  

return ROWS

