local R = require
local _,COLS,the                 = R"lib", R"cols", R"the"
local map,sort,up1,items,push    = _.map, _.sort, _.up1, _.items, _.push
local items,slice,o,oo,sort,many = _.items, _.slice, _.o, _.oo, _.sort, _.many
local class,OBJ                  = _.class, _.OBJ
                
local EGS = class("EGS",OBJ)
function EGS:new() 
  self.rows, self.cols = {}, nil end

function EGS:adds(data)
  for row in items(data) do self:add(row) end
  return self end

function EGS:add(row)
  if not self.cols then self.cols = COLS(row)
                   else push(self.rows, self.cols:add(row)) end end

function EGS.mid(i,cols)
   return map(cols or i.cols.y, function(col) return col:mid() end) end

function EGS:div(cols)
   return map(cols or i.cols.y, function(col) return col:div() end) end

function EGS:clone(rows)
  local out = EGS(self.cols.name)
  for _,row in pairs(rows or {}) do out:add(row) end
  return out end

function EGS:dist(row1,row2)
  local d, n = 0, 0
  for _,col in pairs(self.cols.x) do 
    n = n + 1
    d = d + col:dist(row1[col.at], row2[col.at])^the.p end 
  return (d/n) ^ (1/the.p) end

function EGS:better(row1,row2)
  local s1, s2, n, e = 0, 0, #self.cols.y, math.exp(1)
  for _,col in pairs(self.cols.y) do
    local a = norm(col.lo, col.hi, row1[col.at] )
    local b = norm(col.lo, col.hi, row2[col.at] )
    s1      = s1 - e^(col.w * (a - b) / n)
    s2      = s2 - e^(col.w * (b - a) / n) end
  return s1 / n < s2 / n  end

function EGS:bins(other)
end

function EGS:bestRest()
  self.rows = sort(self.rows, function(a,b) return self:better(a,b) end) 
  local n = (#self.rows)^the.best
  return slice(self.rows, 1,          n),      -- top n things
               many( self.rows, n*the.rest, n+1) end -- some sample of the rest

-- function egs.xplain(i)
--   best, rest = egs.bestRest(i)
--   return egs.contrasts(i, best,rest) end

return EGS 
