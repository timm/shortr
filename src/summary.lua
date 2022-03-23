local R=require
local ako, lib = R"ako", R"lib"
local sym, num = R"sym", R"num"
local norm,push = lib.norm, lib.push

local summary = {}
function summary.new(names)
  return summary.init({names=names, klass=nil,xy= {}, x= {}, y={}},names) end

function summary.init(i, names)
  for at,name in pairs(names) do
    local now = (ako.num(name) and num.new or sym.new)(at,name)
    push(i.xy, now)
    if not ako.ignore(name)  then
      if not ako.goal(name)  then now.indep = true end
      if     ako.klass(name) then i.klass=now      end 
      push(now.indep and i.x or i.y, now)          end end
  return i end

function summary.add(i,row)
  for _,col in pairs(i.xy) do
    (col.nump and num or sym).add(col, row[col.at]) end 
  return row end

function summary.better(i,row1,row2)
  local s1, s2, n, e = 0, 0, #i.y, math.exp(1)
  for _,col in pairs(i.y) do
    local a  = norm(col.lo, col.hi, row1[col.at] )
    local b  = norm(col.lo, col.hi, row2[col.at] )
    s1 = s1 - e^(col.w * (a - b) / n)
    s2 = s2 - e^(col.w * (b - a) / n) end
  return s1 / n < s2 / n  end

return summary
