-- ## Query
local _=require"about"
local R, per, rnd = _.R, _.per, _.rnd
   
local o=require"obj"
local COLS, NUM, ROWS, SOME, SYM = o.COLS, o.NUM,  o.ROWS, o.SOME, o.SYM

--> mids(i:ROW,p:?int=2,cols=?[COL]=i.cols.y):tab -> Return `mid` of columnss
-- rounded to `p` places.
function ROWS.mids(i,p,cols,    t) 
  t={}
  for _,col in pairs(cols or i.cols.y) do t[col.txt]=col:mid(p) end
  return rnd(t,p or 2) end

--> mids(i:(NUM|SYM),p:?float=.5):tab -> Return a columns' `mid`ddle
-- (central tendency), rounded to `p` places.
function NUM.mid(i,p) 
  local a=i.kept:has(); return rnd(per(a,.5),p or 2) end

function SYM.mid(i,p)
  local max,mode=-1,nil
  for x,n in pairs(i.kept) do if n > most then most,mode = n,x end end
  return rnd(mode, p or 2) end

--> div(i:(NUM|SYM),p:?float=.5):tab -> Return `div`ersity of a column
-- (its tendency _not_ to be a its central tendency).
function NUM.div(i,p) 
  local a=i.kept:has(); return rnd( (per(a,.9) - per(a,.1))/2.56, p or 2) end
   
function SYM.div(i,p)
  local ent,log=0,function(x) return math.log(x,2) end
  for x,n in pairs(i.kept) do if n > 0 then ent=ent - n/i.n*log(n/i.n) end end
  return rnd(ent,p or 2) end
-- ### General queries

--> norm(i:NUM, x:num):num -> Normalize `x` 0..1 for lo..hi,
function NUM.norm(i,x)
  local a=i.kept:has(); return (a[#a]-a[1])<1E-9 or (x-a[1])/(a[#a]-a[1]) end

--> has(i:SOME):tab -> Ensure contents are sorted. Return those contents.
function SOME.has(i)
  i.kept = i.ok and i.kept or sort(i.kept); i.ok=true; return i.kept ; end
