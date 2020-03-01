-- vim: ts=2 sw=2 sts=2 et:
--------- --------- --------- --------- --------- ---------

local Columns = require("object"):extend("Columns")
local Num = require("num")
local Sym = require("sym")

function Columns:has() return {
  names= {},
	all  = {},
  nums = {},
  syms = {},
  x    = {all={}, nums={}, syms={}},
  y    = {all={}, nums={}, syms={}, goals={}, klass=nil}}
end

local function usep(x)   return not x:match("%?") end
local function less(x)   return x:match("<") end
local function goalp(x)  return x:match("[<>]") end
local function klassp(x) return x:match("!") end
local function depp(x)   return klassp(x) or goalp(x) end
local function nump(x)   return goalp(x) or x:match("%$") end

function Columns:clone() 
  return self:make(Columns(), self.names) 
end

function Columns:make(   isa,log,xs,ys,new,w)
  local function add(a) a[#a+1] = new end
  for c,x in pairs(self.names) do
    if nump(x) then
      isa,log,xs,ys = Num,self.nums,self.x.nums,self.y.nums
    else
      isa,log,xs,ys = Sym,self.syms,self.x.syms,self.y.syms
    end
    w   = less(x) and -1 or 1
    new = isa {pos= c, txt=x, w=w}
    add(self.all)
    add(log)
    add(depp(x) and ys or xs)
    add(depp(x) and self.y.all or self.x.all)
    if goalp(x)  then add(self.y.goals) end
    if klassp(x) then self.y.klass = new end end
end

return Columns
