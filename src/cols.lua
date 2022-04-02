local R=require
local _, ako, SYM, NUM  = R"lib", R"ako", R"sym", R"num"
local class, OBJ, push = _.class, _.OBJ, _.push

local COLS = class("COLS",OBJ)
function COLS:new(names)
  self.names, self.klass   = names, nil
  self.all, self.x, self.y = {}, {}, {}
  for at,name in pairs(names) do
    local now = push(self.all, (ako.num(name) and NUM or SYM)(at,name))
    if not ako.ignore(name)  then
      if ako.klass(name) then self.klass=now end 
      push(now.indep and self.x or self.y, now) end end end

function COLS:add(row)
  for _,col in pairs(self.all) do col:add(row[col.at]) end
  return row end

return COLS
