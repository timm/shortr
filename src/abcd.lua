#!/usr/bin/env lua
-- vim: ts=2 sw=2 sts=2  et :

--[[

## Compute classifier  performance measures

@include "funny"

To use this code:

1. Create an `Abcd` object.
2. Run your classifier on a test suite. Take the `predicted` and
   `actual` classification and throw it a `Abcd1`.
3. After that, get  a report using `AbcdReport`.

For example suppose:

- Six times, `yes` objects are predicted to be `yes`;
- Twice, a `no` obect is rpedicted to be `no`;
- Five times, `maybe`s are called `maybe`s;
- And once, a `maybe` is called `no`.

After all that,  `AbcdReport` would print:

```
    db |    rx |   num |     a |     b |     c |     d |  acc |  pre |   pd |   pf |    f |    g | class
  ---- |  ---- |  ---- |  ---- |  ---- |  ---- |  ---- | ---- | ---- | ---- | ---- | ---- | ---- |-----
  data |    rx |    14 |    11 |       |     1 |     2 | 0.93 | 0.67 | 1.00 | 0.08 | 0.80 | 0.96 | no
  data |    rx |    14 |     8 |       |       |     6 | 0.93 | 1.00 | 1.00 | 0.00 | 1.00 | 1.00 | yes
  data |    rx |    14 |     8 |     1 |       |     5 | 0.93 | 1.00 | 0.83 | 0.00 | 0.91 | 0.91 | maybe
```

--]]

require "lib"

Abcd={}

function Abcd.new(rx,data) return { 
        known={}, a={}, b={}, c={}, d={}, yes=0, no=0,
        data = data and data or 'data',
        rx   = rx   and rx   or 'rx'}
end

function Abcd.add(i,want,got) 
  if inc(i.known, want) == 1 then i.a[want] = i.yes + i.no end
  if inc(i.known, got)  == 1 then i.a[got]  = i.yes + i.no end
  if want == got then i.yes = i.yes+1 else i.no = i.no + 1 end
  for x,_ in pairs( i.known ) do 
    if   want == x
    then inc(want == got and i.d or i.b, x)
    else inc(got  == x   and i.c or i.a, x)
    end
  end
end

function Abcd.report(i,   p,out,a,b,c,d)
  p = function (z) return math.floor(100*z + 0.5) end
  out= {}
  for x,_ in pairs( i.known ) do
    pd,pf,pn,prec,g,f,acc = 0,0,0,0,0,0,0
    a = i.a[x]; b = i.b[x]; c = i.c[x]; d = i.d[x];
    if b+d > 0          then pd   = d     / (b+d) end
    if a+c > 0          then pf   = c     / (a+c) end
    if a+c > 0          then pn   = (b+d) / (a+c) end
    if c+d > 0          then prec = d     / (c+d) end
    if 1-pf+pd > 0      then g=2*(1-pf) * pd / (1-pf+pd) end 
    if prec+pd > 0      then f=2*prec*pd / (prec + pd) end
    if i.yes + i.no > 0 then acc= i.yes / (i.yes + i.no) end
    out[x] = { data=i.data, rx=i.rx, num = i.yes+i.no,
               a=a, b=b,c=c,d=d, 
               acc=p(acc), prec=p(prec), pd=p(pd), pf=p(pf), 
               f=p(f), g=p(g), class=x}
  end
  return out
end

return {Abcd=Abcd}
