-- ## Update
local _=require"about"
local o=require"obj"
local fmt = _.fmt
local COLS,NUM,SOME,SYM = o.COLS, o.NUM, o.SOME, o.SYM
local ROW,ROWS          = o.ROW, o.ROWS
-- ### Extend Info on 2 Columns

-- extend(i:Bin, x:num, y:(num|str) -> Ensure `lo`,`hi` covers `x`. Add `y` to `ys`.
function Bin.extend(i,x,y)
  i.lo = math.min(i.lo, x)
  i.hi = math.max(i.hi, x)
  ys:summarize(y) end

function Bin.show(i)
  local x,lo,hi = i.ys.txt, i.lo, i.hi
  if     lo ==  hi  then return fmt("%s == %s",x, lo)
  elseif hi ==  big then return fmt("%s >= %s",x, lo)
  elseif lo == -big then return fmt("%s < %s", x, hi)
  else                   return fmt("%s <= %s < %s",lo,x,hi) end end

function Bin.hold(i, row)
  local x = row.cells[i.ys.at]
  return x=="?" or i.lo==i.hi or i.lo<x and x<=i.hi end

function Bin.holds(i, rows)
  return map(rows,function(row) if Bin.hold(i,row) then return row end end) end

local _merge
function Bin.merge(i,j, min)
  local iy,jy = i.ys,j.ys
  local ky    = _merge(iy,jy)
  if iy.n < min or jy.n<min or Col.simpler(ky,iy,jy) then
    return Bin.NEW(i.lo, j.hi, ky) end end

--> simpler(i:col,this:col,that:col):bool->am `i` simpler
-- than `this` and `that`?
function _simpler(i,this,that)
  return i:div(i) <= (this.n*this:div() + that.n*that:div()) / i.n end

function Bin.BINS(rows,col,y,yKlass)
  y      = y or function(row) return row:klass() end
  yKlass = yKlass or Col.NEW
  local n,list, dict = 0,{}, {}
  for _,row in pairs(rows) do
    local v = row.cells[col.at]
    if v ~= "?" then
      n = n + 1
      local pos = Col.bin(col,v)
      dict[pos] = dict[pos] or push(list, Bin(v,v,yKlass:clone()))
      Bin.add(dict[pos], v, y(row)) end end
  list = sort(list, lt"lo")
  list = col.nums and Bin.MERGES(list, small(n)) or list
  return {bins= list,
          div = sum(list,function(z) return Col.div(z.ys)*z.ys.n/n end)} end

function Bin.MERGES(b4, min)
  local n,now = 1,{}
  while n <= #b4 do
    local merged = n<#b4 and Bin.merge(b4[n], b4[n+1], min)
    now[#now+1]  = merged or b4[n]
    n            = n + (merged and 2 or 1)  end
  return #now < #b4 and Bin.MERGES(now,min) or Bin.XPAND(now) end

-- xpand the bins to cover any gaps from minus infinity to plus infinity
function Bin.XPAND(bins)
  if #bins>1 then
    for n=2,#bins do bins[n].lo = bins[n-1].hi end end
  bins[1].lo, bins[#bins].hi = -big, big
  return bins end 



local function _merge(i,j,     k)
  k = i:clone()
  for _,kept in pairs{i.kept, j.kept} do
    for v,inc in pairs(kept) do k:add(v,inc) end end
  return k end


