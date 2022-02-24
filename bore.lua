local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end 
local big = 1E34
local tiny= 1/big

local thing,cli,them,col,num,sym,cols

function thing(x)   
  if x=="true" then return true elseif x=="false" then return false end
  return tonumber(x) or x end

function cli(key,x)
  for n,y in pairs(arg) do if y==k then 
    x=x=="false" and"true" or x=="true" and"false" or arg[n+1] end end
  return thing(x) end

the= {cohen = cli("-c", .35),
      data  = cli("-d", "etc/data/auto93.csv"),
      seed  = cli("-s", 10019)}

function Col(at,x)
  i = {n=0, at=at or 0, txt=txt or "", has={}}
  i.w = i.txt:find"-$" and -1 or 1
  return i end

function Num(at,x)
  i = col(at,t)
  i.is, i.mu,i.m2,i.lo,i.hi= "num",0,0,-big,big
  return i end

function Sym(at,x)
  i = col(at,x)
  i.most=0
  return i end

function Cols(headers,    i,now,here)
  i = {all={}, x={}, y={}}
  for at,x in pair(headers) do
    now = (x:find"^[A-Z]" and Num or Sym)(at,x)
    i.all[at] = now 
    if x:find":$" then
      here =  x:find"[+-]$" and i.y or i.x
      here[1+#where] = col end end
  return i end

function num1(i,x) print(x+1000) end

is={num={add=num1},
    sym={add=sym1}}

function add(i,x) return is[i.is].add(i,x) end

add(num(),10)
print(num().lo)
for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end  
 --
--
-- NUM={is=NUM}
-- local is={}
-- function is.ignorep(x) return x:find":$" end     -- columns to ignore
-- function is.klassp(x)  return x:find"!$" end     -- symbolic goals to achieve
-- function is.lessp(x)   return x:find"-$" end     -- number goals to minimize
-- function is.morep(x)   return x:find"+$" end     -- numeric goals to maximize
-- function is.nump(x)    return x:find"^[A-Z]" end -- numeric columns
-- function is.goalp(x)   return morep(x) or lessp(x) or klassp(x) end
--
-- function col(k,at,txt,t,   i)
-- local DATA,NUM,SYM={},{},{}
-- function NUM.new(k,at,txt) return col(k,at,txt,{mu=0,m2=0,sd=0})  end
-- function NUM.heaven(i,x)   return (i.w - i:norm(x))^2 end
-- function NUM.norm(i,x)     
--   return (i.hi-i.lo)<1E-9 and 0 or (x - i.lo)/(i.hi - i.lo + 1E-9) end
-- function NUM.add(i,x,    d)      
--   if x ~="?" then
--     d    = x - i.mu
--     i.mu = i.mu + d/i.n
--     i.m2 = i.m2 + d*(x - i.mu)
--     i.sd = Num.sd0(i)
--     if x > i.max then i.max = x end
--     if x < i.min then i.min = x end end end
--
-- function SYM.new(k,at,txt) return col(k,at,txt,{}) end
-- function SYM.add(i,x)      i.all[x] = 1 + (i.all[x] or 0) end
--
-- function DATA.new(k,t) 
--   return new(k,{rows={},cols={},x={},y={}}) end
--
-- function DATA.dth(i,t)
--   local fun = function(col) return col:heaven(t[col.at]) end
--   return (sum(i.y, fun)/#i.y)^.5 end
--
-- function DATA.add(i,t)
--   for at,name in pairs(t) do
--     what= (is.nump(name) and NUM or SYM)(at,name)
--     if is.ignorep(x) then
--       
--
-- local the=settings(help)
-- math.randomseed(the.seed)
-- goals={}
-- for n,word in pairs(row) do
--   if is.goalp(word) then
--     goal[n] = is.less[(word) and -1 or 1 end end
--
-- local it= {names={}, cols={}, nums={},x={}, y={}}
--
-- local DATA={}
-- function DATA:new()
--   return new(k,{rows={}, names={}, cols={}, nums={},x={},y={}})
-- function DATA:load(file)  
-- for row in csv(the.data) do
--   if 0==#it.cols then
--     it.names=row
--     for n,x in pairs(has) do
--       col = push(it.cols,{})
--       if not is.ignorep(x) then 
--         if is.nump[n] then it.nums[n]=true end
--         push(is.goalp(x) and it.y or it.x, col) end end
--   else
--     for n,col in pairs(it.cols) do
--       if num
--
--     
--
-- end
