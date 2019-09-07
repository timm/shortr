function ordered(t)
  local i,tmp = 0,{}
  for key,_ in pairs(t) do tmp[#tmp+1] = key end
  table.sort(tmp)
  return function ()
    if i < #tmp then
      i = i+1
      return tmp[i], t[tmp[i]] end end
end

_tostring=tostring
function tostring(a)
  local function go(x,str,sep,seen)
    if type(x) ~= "table" then return _tostring(x) end
    if seen[x] then return "..." end
    seen[x] = true
    for k,v in ordered(x) do
      if not (type(k) == "string" and string.sub(k, 1, 1) == "_") then
        str = str .. sep .. k .. ": " .. go(v,"{","",seen)
        sep = ", "
      end 
    end
    return str .. '}'
  end
  local prefix='{'
  if type(a)=='table' then
    local t = getmetatable(a)
    if t.__index then
      t=t.__index
      prefix = t.__name and t.__name .. '{' or prefix end end 
  return go(a,prefix,"",{})
end

do
  local ids=0
  function identity(o)
    ids = ids + 1
    o._id = ids
    return o
  end
  function ako(o,c,   g)
    g = _G[c]
    g.__name=c
    return identity( setmetatable(o ,{__index=g}))
  end
end


Animal = {}
function Animal.new(o)
  o.name  = o.name  or "name"
  o.sound = o.sound or "sound"
  return ako(o,"Animal")
end

local cat1 = Animal.new{name="one",sound="loud",bday={}}
local cat2 = Animal.new{name="two",sound="loud",bday={}}
local god=Animal.new{name="god"}


getmetatable(cat1).__index.__name ="Bambi"

print("cat1",getmetatable(cat1).__index.__name)
print("cat2",getmetatable(cat2).__index.__name)
print("god",getmetatable(god).__index.__name)

function Animal:respond() -- notice there is no need for self reference here
	print("The " .. self.name .. " will " .. self.sound .. ".\n") -- notice this stays the same
end

cat1:respond() -- notice the use of the colon... Be careful about this.
god:respond() -- notice the use of the colon... Be careful about this.
--dog:respond()
print(cat1)
print(god)

print(2,3)
