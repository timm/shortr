local it={}
it.big = math.huge
it.fmt = string.format
it.R   = math.random

it.b4={}; for x,_ in pairs(_ENV) do it.b4[x]=x end 
function it.lint()
  for x,v in pairs(_ENV) do if not it.b4[x] then print("?",x,type(v)) end end end

function it.thing(str) 
  if type(str)~="string" then return str end
  str = str:match"^%s*(.-)%s*$"
  if str=="true" then return true elseif str=="false" then return false end
  return math.tointeger(str) or tonumber(str) or str  end

function it.help(txt)
  local t={}
  txt:gsub(" [-][-]([^%s]+)[^\n]*%s([^%s]+)",function(key,x) 
    for n,flag in ipairs(arg) do 
      if flag==("-"..key:sub(1,1)) or flag==("--"..key) then 
        x= x=="false" and"true" or x=="true" and"false" or arg[n+1] end end 
      the[key] = it.thing(x) end )
  if t.help then print(txt) end
  return t end 

function it.per(t,p) p=p*#t//1; return t[math.max(1,math.min(#t,p))] end

function it.map(t,f,  u) u={};for _,v in pairs(t) do u[1+#u]=f(v)end;return u end

function it.oo(t) print(o(t)); return t end
function it.o(t) 
  if #t>0 then return "{"..table.concat(lib.map(t,tostring)," ").."}" end
  local u={}; for x,v in pairs(t) do u[1+#u]=lib.fmt(":%s %s",x,v) end
  table.sort(u); return (it.is or "").."{"..table.concat(u," ").."}" end 

function it.obj(name,    t,new)
  function new(kl,...) local x=setmetatable({},kl); kl.new(x,...); return x end 
  t = {__tostring=o, is=name or ""}; t.__index=t
  return setmetatable(t, {__call=new}) end

return it
