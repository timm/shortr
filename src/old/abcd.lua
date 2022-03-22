

-- # abcd.lua
-- 
-- ## Compute classifier  performance measures
-- 
-- To use this code:
-- 
-- 1. Create an `ABCD` object.
-- 2. Run your classifier on a test suite. Take the `predicted` and
--    `actual` classification and throw it a `ABCD1`.
-- 3. After that, get  a report using `ABCDReport`.
-- 
-- For example suppose:
-- 
-- - Six times, `yes` objects are predicted to be `yes`;
-- - Twice, a `no` obect is rpedicted to be `no`;
-- - Five times, `maybe`s are called `maybe`s;
-- - And once, a `maybe` is called `no`.
-- 
-- After all that,  `abcd:report()` would print:
-- 
--     db |    rx |   num |     a |     b |     c |     d |  acc |  pre |   pd |   pf |    f |    g | class
--   ---- |  ---- |  ---- |  ---- |  ---- |  ---- |  ---- | ---- | ---- | ---- | ---- | ---- | ---- |-----
--   data |    rx |    14 |    11 |       |     1 |     2 | 0.93 | 0.67 | 1.00 | 0.08 | 0.80 | 0.96 | no
--   data |    rx |    14 |     8 |       |       |     6 | 0.93 | 1.00 | 1.00 | 0.00 | 1.00 | 1.00 | yes
--   data |    rx |    14 |     8 |     1 |       |     5 | 0.93 | 1.00 | 0.83 | 0.00 | 0.91 | 0.91 | maybe
-- 
local _,the = require"tricks", require"the"
local class,fmt,inc,lines,new = _.class, _.fmt, _.inc, _.lines, _.new
local o,oo,push,slots         = _.o, _.oo, _.push, _.slots

-- ## Class
local ABCD = class"ABCD"

function ABCD:new(data,rx)
  return new({data= data or "data", rx= rx or "rx",known={},
              a={}, b={}, c={}, d={}, yes=0, no=0}, ABCD) end

function ABCD.exists(i,x,   new) 
  new = not i.known[x]
  inc(i.known,x)
  if new then
    i.a[x]=i.yes + i.no; i.b[x]=0; i.c[x]=0; i.d[x]=0 end end

function ABCD.add(i,want,got) 
  i:exists(want) 
  i:exists(got)  
  if want==got then i.yes=i.yes+1 else i.no=i.no+1 end
  for x,_ in pairs(i.known) do 
    if   want == x
    then inc(want == got and i.d or i.b, x)
    else inc(got  == x   and i.c or i.a, x) end end end

function ABCD.report(i,   p,out,a,b,c,d,pd,pf,pn,f,acc,g,prec)
  p = function (z) return math.floor(100*z + 0.5) end
  out= {}
  for x,_ in pairs( i.known ) do
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
    out[x] = { data=i.data, rx=i.rx, 
               num = i.yes+i.no, a=a, b=b,c=c,d=d, 
               acc=p(acc), prec=p(prec), pd=p(pd), pf=p(pf), 
               f=p(f), g=p(g), class=x} end
  return out end

function ABCD.show(i,report,     s,d,t)
  print""
  d,s = "---", "%10s | %10s | %4s | %4s | %4s | %4s | %3s | %3s| %3s | %4s | %3s | %3s |"
  print(fmt(s,"db","rx","a","b","c","d","acc","pd","pf","prec","f","g"))
  print(fmt(s,d,d,d,d,d,d,d,d,d,d,d,d))
  report = report or i:report()
  for _,x in pairs(slots(report)) do
    t = report[x]
    print(fmt(s.." %s", t.data,t.rx,t.a, t.b, t.c, t.d,t.acc, t.pd, t.pf, t.prec, t.f, t.g, x)) end end

return ABCD
