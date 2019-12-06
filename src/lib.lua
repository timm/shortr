-- vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro :
--------- --------- --------- --------- --------- --------- 

local THE=require("the").misc
local Lib={}

-- Numbers
local Rand=require("rand")
Lib.r = Rand.r

function Lib.round(num, places)
  local mult = 10^(places or 0)
  return math.floor(num * mult + 0.5) / mult
end

function Lib.within(a,b,c) return b>=a and b<=c end

-- ---------------------------------
-- Meta
function Lib.same(x) return x end

-- ---------------------------------
-- Lists
function Lib.last(a) return a[#a] end

function Lib.sort(a,f) 
  f = f or function(x,y) return x<y end
  table.sort(a,f) 
  return a 
end

function Lib.mean(a,       n,sum) 
  n,sum=0,0
  for _,one in pairs(a) do n=n+1; sum= sum+ one end 
  return sum/n
end

-- -------------------------
-- Print tables.
-- 
-- Convert tables to strings (and do so recursively).
-- Work over the tables in alphabetically order of the keys.
-- Skip entries that are _private_ (that start with `_`)
-- or which point to functions.
-- If all the indexes are numeric,
-- then do not show the keys. 

function Lib.o(t) print(Lib.oo(t))  end

function Lib.oo(t,     s,sep,keys, nums)
  if type(t) ~= "table" then return tostring(t) end
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
    v   = type(v) == 'table' and Lib.oo(v) or tostring(v) 
    if nums
    then s = s .. sep .. v
    else s = s .. sep .. tostring(k) .. '=' .. v
    end
    sep = ', ' 
  end 
  return  '{' .. s ..'}'
end

-- -----------------
-- And finally...

return Lib
