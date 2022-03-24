local lib=require"lib"
local bin=require"bin"
local map,push,sort = lib.map, lib.push, lib.sort

local _={}
function _.new(bins,   t)
  t = {}
  for key,one in pairs(bins) do t[one.at]=t[one.at] or {}; push(t[one.at],one) end 
  return {bins=t} end

function _.selects(i,row)
  local function ors(bins)
    for key,x in pairs(bins) do if bin.select(x,row) then return true end end
    return false end
  for at,bins in pairs(i.bins) do if not ors(bins) then return false end end
  return true end 

function _.show(i,bins)
  local cat, order, ors
  cat =  function(t,sep) return table.concat(t,sep) end
  order= function(a,b)  return a.lo < b.lo end
  ors=   function(bins) 
          return cat(map(bin.Merges(sort(bins,order)),bin.show)," or ") end
  return cat(map(i.bins, ors)," and ") end

return _
