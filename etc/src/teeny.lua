local the,help = {},[[
TODO: dont fuse on non-numerics

 -c cohen difference in nums    = .35
 -f file  source                = ../../data/auto93.csv
 -g go    action                = help
 -m min   size of small         = .5
 -s seed  random number seed    = 10019]]

big=math.huge 
min=math.min
max=math.max
fmt=string.format
rand=math.random

function shuffle(t,   j)
  for i=#t,2,-1 do j=rand(i); t[i],t[j]=t[j],t[i] end; return t end

function coerce(x)
  x = x:match"^%s*(.-)%s*$" 
  if x=="true" then return true elseif x=="false" then return false 
  else return math.tointeger(x) or tonumber(x) or x end  end

help:gsub("\n ([-]%S)[%s]+([%S]+)[^\n]+= ([%S]+)",function(flag,key,x) 
  for n,arg1 in ipairs(arg) do 
    if   arg1==flag
    then x = x=="false" and "true" or x=="true" and "false" or arg[n+1] end end
  the[key] = coerce(x) end) 
 
local all={}
function all.table(t,fun) for k,v in pairs(t) do fun(t) end end

function all.csv(file,fun)
  function lines(file, fun)
    local file = io.input(file)
    while true do
      local line = io.read()
      if not line then return io.close(file) else fun(line) end end 
  end -----------------------------
  function words(s,sep,fun,      t)
     fun = fun or same
     t={};for x in s:gmatch(fmt("([^%s]+)",sep)) do t[1+#t]=fun(x) end; return t 
  end -------------------------------------------------------------
  lines(file, function(line) fun(words(line, ",", coerce)) end) end 

function main(all,src,fun) -- all.csv,file or all.table,rows
  local data={rows={}, names={},nums = nil}
  all(src, function(row)
    if not data.nums then
      for k,v in pairs(row) do 
        if v:find"^[A-Z]" then data.nums[k]={lo=big,hi=-big} end end
    else
      for k,n in pairs(data.nums) do 
        if x~="?" then n.lo=min(n.lo,x); n.hi=max(n.hi,x) end end 
      data.rows[ 1+#i.rows ] = row 
      if (#rows> the.wait) then 
        shuffle(data.rows)
        fun(data)
        data.rows={} end end end) end 
