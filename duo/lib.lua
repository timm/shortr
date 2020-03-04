-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Lib={}

function Lib.list() return {} end

function Lib.map(a,f,     b)
  b = {}
  f = f or function(z) return z end
  for i,v in pairs(a or {}) do b[i] = f(v) end 
  return b
end 

function Lib.copy(t) 
  return type(t) ~= 'table' and t or Lib.map(t,Lib.copy) 
end

function Lib.o(x) print(Lib.oo(x)) end

local function is_array(t)
  local i = 0
  for _ in pairs(t) do
    i = i + 1
    if t[i] == nil then return false end end
  return true
end

local function sortAnything(x,y)
  if   type(x)=="number" and type(y)=="number" 
  then return x < y
  end
  return tostring(x) < tostring(y)
end

function Lib.oo(t,     seen,s,sep,keys, nums)
  seen = seen or {}
  if seen[t] then return "..." end
  local pre=pre or (t.class and t.class.name or "")
  seen[t] = true
  if type(t) ~= "table" then return tostring(t) end
  s, sep, keys, nums = '','', {}, true
  for k, v in pairs(t) do 
    if k            ~= 'class' and
       k            ~= 'super' and
       not (type(v) == 'function') and
       not (type(k) == 'string' and k:match("^_")) 
    then
       nums = nums and type(k) == 'number'
       keys[#keys+1] = k 
    end 
  end
  table.sort(keys,sortAnything)
  for _, k in pairs(keys) do
    local v = t[k]
    if      type(v) == 'table'    then v= Lib.oo(v, seen) 
    elseif  type(v) == 'function' then v= "function"
    else v= tostring(v) 
    end
    if nums
    then s = s .. sep .. v
    else s = s .. sep .. tostring(k) .. '=' .. v
    end
    sep = ', ' 
  end 
  return tostring(pre) .. '{' .. s ..'}'
end

return Lib
