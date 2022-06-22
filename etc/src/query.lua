-- ## Query
local _=require"about"
local kl=require"klass"
local R, csv, per, push, rnd = _.R, _.csv, _.per, _.push, _.rnd
local COLS, NUM, SOME, SYM   = kl.COLS, kl.NUM, kl.SOME, kl.SYM
local ROW,ROWS               = kl.ROW, kl.ROWS
-- ### mid(s)

--> mids(i:ROW,p:?int=2,cols=?[COL]=i.cols.y):tab -> Return `mid` of columnss
-- rounded to `p` places.
function ROWS.mids(i,p,cols,    t) 
  t={}
  for _,col in pairs(cols or i.cols.y) do t[col.txt]=col:mid(p) end
  return rnd(t,p or 2) end

--> mids(i:ROW,p:?float=.5,cols=?[COL]=i.cols.y):tab -> Return `mid` of columns.
-- rounded to `p` places.
function SYM.mid(i,p)
  local max,mode=-1,nil
  for x,n in pairs(i.kept) do if n > most then most,mode = n,x end;
  return rnd(mode, p or 2) end

function NUM.mid(i,p) local a=i.kept:has(); return rnd(per(a,.5),p or 2) end
-- ### div

function NUM.div(i,p) 
  local a=i.kept:has(); return rnd( (per(a,.9) - per(a,.1))/2.56, p or 2) end

function SYM.div(i,p)
  local ent,log=0,function(x) return math.log(x,2) end
  for x,n in pairs(i.kept) do if n > 0 then ent=ent - n/i.n*log(n/i.n) end end
  return rnd(ent,p or 2) end
-- ### Other queries
function NUM.norm(i,x)
  local a=i.kept:has(); return (a[#a]-a[1])<1E-9 or (x-a[1])/(a[#a]-a[1]) end

function SOME.has(i)
  i.kept = i.ok and i.kept or sort(i.kept); i.ok=true; return i.kept end


