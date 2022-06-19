local _ = require("lib")
local R, per = _.R, _.per

local Some=klass"Some"
function Some.NEW(i) 
  i._is=Some; i.n=0; i.kept={}; i.ok=trues;, i.max=the.some end

function Some.add(i,x,n)
  for _ =1,n do
    if     #i.kept < i.max  then i.ok=false;push(i.kept,x) 
    elseif R() < i.nums/i.n then i.ok=false;i.kept[R(#i.kept)]=x end end end 

function Some.has(i)
  if not i.ok then table.sort(i.kept); i.ok=true; return i.kept end end

return Some
