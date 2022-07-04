-- ## Lua to Narkdiwn
local all=require"all"
local chunkscat,lines,push = all.cat, all.lines, all.push

-- asdas
-- asdasaa  asdas
-- adasa s

-- asda

local doc={}
function doc.chunks(file)
  local prep=function(t) 
    if t[#t]:find"^[%s]*$" then t[#t]=nil end; return table.concat(t,"\n") end
  local b4,now,t,out=0, 0,{},{}
  lines(file, function(s)
    now=b4
    if s:sub(1,3)=="-- " then now=0; s=s:sub(4) elseif s:find"^%S" then now=1 end
    if now==b4 then push(t,s) else push(out, {what=now, txt=prep(t)}) ; t={s} end
    b4 = now end)
  if #t>0 then push(out,{what=now, txt=prep(t)}) end
  return out end

for n,chunk in pairs(doc.chunks("doc.lua")) do print(""); print(n,chunk.what,"[["..chunk.txt.."]]") end

asdas=2

-- asdas
-- saas
asdas = asda

