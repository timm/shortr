local class=require "30log"
local Window = class("Window") 

Window.width = 5000
Window.height = -100000

function Window:init(t)
  return self:inits(t or {})
end

function Window:inits(t)
  self.width  = t.width or 200
  self._m=22
  self.height = t.height or 100000
end

function Window:cap()
  self.width  = math.min(self.width, Window.width)
  self.height = math.min(self.height, Window.height)
end

function Window.__tostring(self)
  self = self or {}
  local all={}
  for k,v in pairs(self) do
    if  k ~= "class" and k ~= "super" then
      if "_" ~= string.sub(k, 1, 1) then
        all[#all+1] = k end end end
  table.sort(all)
  local s,sep = "",""
  for k=1,#all do
    s = s .. sep .. all[k] .. "=" .. tostring(self[all[k]]) 
    sep=", " 
  end 
  return (self.name or 'Object') .. "{" .. s .. "}"
end

local x = Window{width=729}
x:cap()

print(x.width, x.height) -- outputs 720, 480
print(x)

local Frame = Window:extend()
 
function Frame:inits(t)
   Frame.super.inits(self, t)
   self.color = t.color or "blue"
 end

y=Frame{width=10000000,height=-100000000}
z=Frame()
y:cap()
z:cap()

print(y)
print(z)
