-- vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro :
--------- --------- --------- --------- --------- --------- 

local THE=require("the").misc

math.randomseed(THE.seed)
r = math.random

function within(a,b,c) return b>=a and b<=c end

function round(x) return math.floor( x + 0.5 ) end


-- Convert tables to strings (and do so recursively).
-- Work over the tables in alphabetically order of the keys.
-- Skip entries that are _private_ (that start with `_`)
-- or which point to functions.
-- If all the indexes are numeric,
-- then do not show the keys. 
function o(t) print(oo(t))  end

function oo(t,     s,sep,keys, nums)
  s, sep, keys, nums = '','', {}, true
  for k, v in pairs(t) do 
    if type(v) ~= 'function' then
      if not (type(k)=='string' and k:match("^_")) then
        nums = nums and type(k) == 'number'
        keys[#keys+1] = k  end end
  end 
  table.sort(keys)
  for _, k in pairs(keys) do
    local v = t[k]
    v   = type(v) == 'table' and oo(v) or tostring(v) 
    if nums
    then s = s .. sep .. v
    else s = s .. sep .. tostring(k) .. '=' .. v
    end
    sep = ', ' 
  end 
  return  '{' .. s ..'}'
end


