local lib=require"lib"
local fmt=lib.fmt

local abcd={}

function abcd.new(data,rx) 
  return {data= data or "data",rx= rx or "rx",
         known={},a={},b={},c={},d={},yes=0,no=0} end

function abcd.exists(i,x,   new) 
  new = not i.known[x]
  lib.inc(i.known,x)
  if new then
    i.a[x]=i.yes + i.no; i.b[x]=0; i.c[x]=0; i.d[x]=0 end end

function abcd.report(i,    p,out,a,b,c,d,pd,pf,pn,f,acc,g,prec)
  p = function (z) return math.floor(100*z + 0.5) end
  out= {}
  for x,xx in pairs( i.known ) do
    pd,pf,pn,prec,g,f,acc = 0,0,0,0,0,0,0
    a= (i.a[x] or 0); b= (i.b[x] or 0); c= (i.c[x] or 0); d= (i.d[x] or 0);
    if b+d > 0     then pd   = d     / (b+d)        end
    if a+c > 0     then pf   = c     / (a+c)        end
    if a+c > 0     then pn   = (b+d) / (a+c)        end
    if c+d > 0     then prec = d     / (c+d)        end
    if 1-pf+pd > 0 then g=2*(1-pf) * pd / (1-pf+pd) end 
    if prec+pd > 0 then f=2*prec*pd / (prec + pd)   end
    if i.yes + i.no > 0 then 
       acc= i.yes / (i.yes + i.no) end
    out[x] = {data=i.data,rx=i.rx,num=i.yes+i.no,a=a,b=b,c=c,d=d,acc=p(acc),
              prec=p(prec), pd=p(pd), pf=p(pf),f=p(f), g=p(g), class=x} end
  return out end

function abcd.pretty(t)
  print""
  local s1  = "%10s | %10s | %4s | %4s | %4s | %4s "
  local s2  = "| %3s | %3s| %3s | %4s | %3s | %3s |"
  local d,s = "---", (s1 .. s2)
  print(fmt(s,"db","rx","a","b","c","d","acc","pd","pf","prec","f","g"))
  print(fmt(s,d,d,d,d,d,d,d,d,d,d,d,d))
  for key,x in pairs(lib.slots(t)) do
    local u = t[x]
    print(lib.fmt(s.." %s", u.data,u.rx,u.a, u.b, u.c, u.d,
                              u.acc, u.pd, u.pf, u.prec, u.f, u.g, x)) end end

function abcd.adds(gotwants, show,data, rx)
  local i = abcd.new(data,rx)
  for key,one in pairs(gotwants) do 
    abcd.exists(i,one.want) 
    abcd.exists(i,one.got)  
    if one.want == one.got then i.yes=i.yes+1 else i.no=i.no+1 end
    for x,xx in pairs(i.known) do 
      if   one.want == x
      then lib.inc(one.want == one.got and i.d or i.b, x)
      else lib.inc(one.got  == x       and i.c or i.a, x) end end end 
  return show and abcd.pretty(abcd.report(i)) or abcd.report(i) end

return abcd.adds
