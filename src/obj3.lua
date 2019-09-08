require "lib"

Animal = {}
function Animal.new(o)
  o.name  = o.name  or "name"
  o.sound = o.sound or "sound"
  return ako(o,"Animal")
end

local cat1 = Animal.new{name="one",sound="loud",bday={}}
local cat2 = Animal.new{name="two",bday={}}
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
