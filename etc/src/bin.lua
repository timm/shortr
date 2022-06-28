--  ##  info on 2 cols
local all=require"all"
local big,fmt,lt,obj = all.big,all.fmt,all.lt,all.obj,all
local small,sort = all.small,all.sort

--> BIN(xlo:num,xhi:num,ys:(NUM|SYM)):BIN ->
-- `ys` stores values seen from `xlo to `xhi`.
local BIN = obj("BIN", function(xlo, xhi, ys)
  i.lo, i.hi, i.ys = xlo, xhi, ys end)

-- add(i:Bin, x:num, y:(num|str) -> Ensure `lo`,`hi` covers `x`. Add `y` to `ys`.
function BIN.add(i,x,y)
  i.lo = math.min(i.lo, x)
  i.hi = math.max(i.hi, x)
  ys:add(y) end

function BIN.hold(i, row)
  local x = row.cells[i.ys.at]
  if x=="?" or i.lo==i.hi or i.lo<x and x<=i.hi then return row end end

function BIN.holds(i, rows)
  return map(rows, function(row) return i:hold(row) end) end

function BIN.merged(i,j, min)
  local a, b, c = i.ys, j.ys, i.ys:merge(j.ys)
  if a.n < min or b.n < min or c:div() <= (a.n*a:div() + b.n*b:div())/c.n then
    return BIN(i.lo, j.hi, c) end end

function BIN.show(i)
  local x,lo,hi = i.ys.txt, i.lo, i.hi
  if     lo ==  hi  then return fmt("%s == %s", x, lo)
  elseif hi ==  big then return fmt("%s >  %s", x, lo)
  elseif lo == -big then return fmt("%s <= %s", x, hi)
  else                   return fmt("%s <  %s <= %s", lo,x,hi) end end

-- ----
function BIN.BINS(rows,col,yKlass,y)
  y      = y or function(row) return row:klass() end
  yKlass = yKlass or SYM
  local n,list, dict = 0,{}, {}
  for _,row in pairs(rows) do
    local v = row.cells[col.at]
    if v ~= "?" then
      n = n + 1
      local pos = col:bin(v)
      dict[pos] = dict[pos] or push(list, BIN(v,v,yKlass(col.at, col.txt)))
      dict[pos]:add(v, y(row)) end end
  list = col:merges(sort(list, lt"lo"), small(the.Min, n))
  return {bins= list,
          div = sum(list,function(z) return z.ys:div()*z.ys.n/n end)} end
