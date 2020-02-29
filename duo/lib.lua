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

function Lib.o(x,pre) print(Lib.oo(x,pre)) end

local function is_array(t)
  local i = 0
  for _ in pairs(t) do
    i = i + 1
    if t[i] == nil then return false end
  end
  return true
end

function Lib.oo(x,t)
  local pre      = t.pre or ""
  local width    = t.width or 5
  local decimals = t.decimals or 3
  local fmt      = t.fmt or string.format("%%%s.%sf",width,decimals)
  local trim     = function (s) return s:match "^%s*(.-)%s*$" end
  if type(x) == 'function' then return 'FUN' end
  if type(x) == 'number'   then 
     local s1=trim(string.format(fmt,x))
     local s2=trim(string.format("%s",x))
     return #s2 <= #s1 and s2 or s1
  end
  if type(x) ~= 'table'    then return tostring(x) end
  local sep,s="",(pre .. "{")
  t.pre=""
  if is_array(x) then
    for k,v in pairs(x) do
      s   = s .. sep ..  Lib.oo(v,t)
      sep = ", "
    end 
  else
    for k,v in pairs(x) do
      s   = s .. sep .. tostring(k) .. ":" .. Lib.oo(v,t)
      sep = ", "
    end
  end
  return s .."}"
end

return Lib
