#!/usr/bin/env lua
-- vim: ts=2 sw=2 sts=2  et :

--[[

## Compute classifier  performance measures

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

local Abcd={ako="Abcd"}

function Abcd.new(o)
  o.data = o.data or "data"
  o.rx   = o.rx   or "rx"
  o.known, o.a, o.b, o.c, o.d = {}, {}, {}, {}, {}
  o.yes, o.no = 0, 0
  return isa(o,Abcd)
end

function Abcd:exists(x) 
  if inc(self.known,x) == 1 then 
    self.a[x] = self.yes + self.no
    self.b[x] = 0
    self.c[x] = 0
    self.d[x] = 0
  end
end

function Abcd:add(want,got) 
  self:exists(want) 
  self:exists(got)  
  if   want==got 
  then self.yes= self.yes + 1 
  else self.no = self.no  + 1 
  end
  for x,_ in pairs( self.known ) do 
    if   want == x
    then inc(want == got and self.d or self.b, x)
    else inc(got  == x   and self.c or self.a, x)
    end
  end
end

function Abcd:report(   p,out,a,b,c,d,pd,pf,pn,f,acc,g,prec)
  p = function (z) return math.floor(100*z + 0.5) end
  out= {}
  for x,_ in pairs( self.known ) do
    pd,pf,pn,prec,g,f,acc = 0,0,0,0,0,0,0
    a= self.a[x]; b= self.b[x]; c= self.c[x]; d= self.d[x];
    if b+d > 0     then pd   = d     / (b+d)        end
    if a+c > 0     then pf   = c     / (a+c)        end
    if a+c > 0     then pn   = (b+d) / (a+c)        end
    if c+d > 0     then prec = d     / (c+d)        end
    if 1-pf+pd > 0 then g=2*(1-pf) * pd / (1-pf+pd) end 
    if prec+pd > 0 then f=2*prec*pd / (prec + pd)   end
    if self.yes + self.no > 0 then 
       acc= self.yes / (self.yes + self.no) end
    out[x] = { data=self.data, rx=self.rx, 
               num = self.yes+self.no, a=a, b=b,c=c,d=d, 
               acc=p(acc), prec=p(prec), pd=p(pd), pf=p(pf), 
               f=p(f), g=p(g), class=x}
  end
  return out
end

return Abcd
