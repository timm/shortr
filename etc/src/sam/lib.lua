local  lib={}
local.big=math.huge
local fmt=string.format
local R=math.random

lib.b4={}; for x,_ in pairs(_ENV) do lib.b4[x]=x end 
function lib.lint()
  for x,v in pairs(_ENV) do if not lib.b4[x] then print("?",x,type(v)) end end end


function lib.thing(str) 
  if type(str)~="string" then return str end
  str = str:match"^%s*(.-)%s*$"
  if str=="true" then return true elseif str=="false" then return false end
  return math.tointeger(str) or tonumber(str) or str  end

function lib.help(txt)
  local t={}
  txt:gsub(" [-][-]([^%s]+)[^\n]*%s([^%s]+)",function(key,x) 
    for n,flag in ipairs(arg) do 
      if flag==("-"..key:sub(1,1)) or flag==("--"..key) then 
        x= x=="false" and"true" or x=="true" and"false" or arg[n+1] end end 
      the[key] = lib.thing(x) end )
  if t.help then print(txt) end
  return t end 

local All={}
local function klass(x) All[x]=All[x] or {}; return All[x] end

return lib
