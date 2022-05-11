big=math.huge
rand=math.random
fmt=string.format
function map(t,f, u)  u={};for k,v in pairs(t) do u[1+#u]=f(v) end; return u end
function push(t,x)    t[1+#t]=x; return x end
function slice(t,i,j,k,     u) 
  i,j = (i or 1)//1, (j or #t)//1
  k   = (k and (j-i)/k or 1)//1
  u={}; for n=i,j,k do u[1+#u] = t[n] end return u end

-- "Strings 2 things" coercion. 
function string2thing(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
  return math.tointeger(x) or tonumber(x) or x  end

function csv(csvfile) 
  csvfile = io.input(csvfile)
  return function(line, row) 
    line=io.read()
    if not line then io.close(csvfile) else
      row={}; for x in line:gmatch("([^,]+)") do push(row,string2thing(x)) end
      return row end end end 

-- "Things 2 strings" coercion.
function oo(t) print(o(t)) end
function o(t,    u)
  if #t>0 then return "{"..table.concat(map(t,tostring)," ").."}" else
    u={}; for k,v in pairs(t) do u[1+#u] = fmt(":%s %s",k,v) end
    return (t.is or "").."{"..table.concat(sort(u)," ").."}" end end

function rnds(t,f) return map(t, function(x) return rnd(x,f) end) end
function rnd(x,f) 
  return fmt(type(x)=="number" and (x~=x//1 and f or the.rnd) or"%s",x) end

function is_skip(x) return x:find":$"     end
function is_goal(x) return x:find"[-+!]$" end
function is_num(x)  return x:find"^[A-Z]" end

function ROW(cells) return {cooked={}, cells=cells} end
function COLS()     return {names={},  all=xy(), num=xy(), sym=xy{}} end
function EGS()      return {rows={},   cols=nil} end
function XY()       return {all={},    x={}, y={}} end

function cols(names)
  i=COLS()
  i.names=names
  for at,txt in pairs(names) do
    for _,slot in pairs{i.all,is_num(txt) and i.num or i.sym} do 
      push(slot.all, at)
      if not is_skip(txt) then 
        push(is_goal(txt) and slot.y or slot.x, at) end end end
  return i end

function w(i,row,   lo,hi)
  lo,hi,x = {},{}
  if not i.cols0 then 
    i.cols =COLS(row)
    for _,at in pairs(i.cols.nums.all) do lo[at],hi[at] = 1E32, -1E32 end
  else
    push(i.rows,ROW(row))
    for _,at in pairs(i.cols.nums.all) do 
      if row[at] ~="?" then 
        lo[at] = math.min(lo[at], row[at])
        hi[at] = math.max(hi[at], row[at]) end end end end
    



