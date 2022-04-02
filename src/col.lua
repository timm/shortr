local ako, _ = require"ako", require"lib"
local class, OBJ =  _.class, _.OBJ
local o,oo = _.o, _.oo

local COL = class("COL",OBJ)
function COL:new(at,name)
   self.at, self.name = at or 0, name or ""
   self.n = 0
   self.indep = not ako.goal(self.name)
   self.nump  = ako.num(self.name)
   self.w = self.name:find"-$" and -1 or 1 end

function COL:adds(t) 
  for _,x in pairs(t) do self:add(x) end; return self end

function COL:add(x,inc)
  if x ~= "?" then
    inc = inc or 1
    self.n = self.n + inc
    self:add1(x,inc) end
  return x end

function COL:dist(x,y)
  return x=="?" and y=="?" and 1 or self:dist1(x,y) end 

function COL:merged(other,    out)
  out = self:merge(other)
  if out:div()*.95 <= (self.n*self:div() + other.n*other:div())/out.n then
    return out end end

return COL
