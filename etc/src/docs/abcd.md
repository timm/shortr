<img align=left width=250   src="bat2.png">

# B(Ai)ttery
LUA is a "batteries-not-included" language.
But LUA makes it easy to add in the  missing bits.
E.g. here are some "b(Ai)tteries" for XAI.

(c) 2022, Tim Menzies<br><timm@ieee.org>
<br clear=all>

**config:** [all](all.html);
**build:** [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile)
(just for doco);
**demos:** [go](go.html);
**apps:** [nb](nb.html);
**functions:** [lib](lib.html)    
**klasses:** [bin](bin.html)
:: [cols](cols.html)
:: [num](num.html)
:: [row](row.html)
:: [rows](rows.html)
:: [some](some.html)
:: [sym](sym.html)

<img src="https://img.shields.io/badge/Language--lua-%232C2D72.svg?logo=lua&logoColor=white"> <a
href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 


abcd.lua
## Categorical performance stats


<details><summary></summary>

```lua
local all = require"all"
local fmt, obj = all.fmt, all.obj

local ABCD = obj("ABCD", function (i,data,rx)
  i.data, i.rx =data or "", rx or ""
  i.yes, i.no = 0,0
  i.known,i.a,i.b,i.c,i.d = {},{},{},{},{} end)


local function inc(f,a,n) f=f or {};f[a]=(f[a] or 0) + (n or 1) return f end
local function slots(t, u)
  u={}; for k,v in pairs(t) do u[1+#u]=k end; table.sort(u); return u end

function ABCD.exists(i,x)
  local new = not i.known[x]
  inc(i.known,x)
  if new then
    i.a[x]=i.yes + i.no; i.b[x]=0; i.c[x]=0; i.d[x]=0 end end

function ABCD.report(i,    p,out,a,b,c,d,pd,pf,pn,f,acc,g,prec)
  p = function (z) return math.floor(100*z + 0.5) end
  out= {}
  for x,xx in pairs( i.known ) do
    pd,pf,pn,prec,g,f,acc = 0,0,0,0,0,0,0
    a= (i.a[x] or 0); b= (i.b[x] or 0); 
    c= (i.c[x] or 0); d= (i.d[x] or 0);
    if b+d > 0     then pd   = d     / (b+d)        end
    if a+c > 0     then pf   = c     / (a+c)        end
    if a+c > 0     then pn   = (b+d) / (a+c)        end
    if c+d > 0     then prec = d     / (c+d)        end
    if 1-pf+pd > 0 then g=2*(1-pf) * pd / (1-pf+pd) end 
    if prec+pd > 0 then f=2*prec*pd / (prec + pd)   end
    if i.yes + i.no > 0 then 
       acc= i.yes /(i.yes + i.no) end
    out[x] = {data=i.data,rx=i.rx,num=i.yes+i.no,
              a=a,b=b,c=c,d=d,acc=p(acc),
              prec=p(prec), pd=p(pd), pf=p(pf),f=p(f), g=p(g), class=x} end
  return out end

function ABCD.pretty(i,t)
  print""
  local s1  = "%10s | %10s | %4s | %4s | %4s | %4s "
  local s2  = "| %3s | %3s| %3s | %4s | %3s | %3s |"
  local d,s = "---", (s1 .. s2)
  print(fmt(s,"db","rx","a","b","c","d","acc","pd","pf","prec","f","g"))
  print(fmt(s,d,d,d,d,d,d,d,d,d,d,d,d))
  for key,x in pairs(slots(t)) do
    local u = t[x]
    print(fmt(s.." %s", u.data,u.rx,u.a, u.b, u.c, u.d,
                         u.acc, u.pd, u.pf, u.prec, u.f, u.g, x)) end end

function ABCD.add(i,got,want)
  i:exists(want) 
  i:exists(got)  
  if want == got then i.yes=i.yes+1 else i.no=i.no+1 end
  for x,xx in pairs(i.known) do 
    if   want == x
    then inc(want == got and i.d or i.b, x)
    else inc(got  == x   and i.c or i.a, x) end end end 

return ABCD
```

</details>

