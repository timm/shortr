local bin={}
local the=require"the"
local lib=require"lib"
local fmt,per,push,sort = lib.fmt, lib.per, lib.push, lib.sort

function bin.new(id,at,name,lo,hi,n,div) 
  return {id=id,at=at,name=name,lo=lo,hi=hi,n=n,div=div} end

function bin.show(i,negative)
  local x,lo,hi,big, s = i.name, i.lo, i.hi, math.huge
  if negative then
    if     lo== hi  then s=fmt("%s != %s",x,lo)  
    elseif hi== big then s=fmt("%s <  %s",x,lo) 
    elseif lo==-big then s=fmt("%s >= %s",x,hi)  
    else                 s=fmt("%s < %s and %s >= %s",x,lo,x,hi) end 
  else
    if     lo== hi  then s=fmt("%s == %s",x,lo)  
    elseif hi== big then s=fmt("%s >= %s",x,lo)  
    elseif lo==-big then s=fmt("%s <  %s",x,hi)  
    else                 s=fmt("%s <= %s < %s",lo,x,hi) end end
  return s end

function bin.select(i,row)
  local x, lo, hi = row[i.at], i.lo, i.hi
  return x=="?" or lo == hi and lo == x or lo <= x and x < hi end

function bin.Merges(bins)
  local j,n,new = 0,length(bins),{}
  while j <= n do
    j=j+1
    a=bins[j]
    if j < n then
      b = bins[j+1]
      if a.hi == b.lo then
        a.hi  = b.hi
        a.div = (a.div*a.n + b.div*b.n)/(a.n+b.n)
        a.n   = a.n + b.n
        j     = j + 1 end end
    push(new,a) end
  return #new < #bins and bin.Merges(new) or bins end

local _argmin
function bin.Xys(xys,at,name)
  xys                  = sort(xys, upx)
  local triviallySmall = the.cohen*(per(xys,.9).x - per(xys, .1).x)/2.56 
  local enoughItems    = #xys / the.bins
  local out            = {}
  _argmin(1,#xys, xys, triviallySmall, enoughItems, -math.huge, at.name, out)
  out[#out].hi =  math.huge 
  return out end

function _argmin(lo, hi, xys, triviallySmall, enoughItems, b4, at, name,out)
  local function add(f,z) f[z] = (f[z] or 0) + 1 end
  local function sub(f,z) f[z] =  f[z] - 1       end
  local lhs, rhs, cut, div, xpect, xy = {},{}
  for j=lo,hi do add(rhs, xys[j].y) end
  div = ent(rhs)
  if hi-lo+1 > 2*enoughItems then
    for j=lo,hi - enoughItems do
      add(lhs, xys[j].y)
      sub(rhs, xys[j].y)
      local n1,n2 = j - lo +1, hi-j
      if   n1        > enoughItems and        
           n2        > enoughItems and       
           xys[j].x ~= xys[j+1].x and  -- there is a break here
           xys[j].x  - xys[lo].x > triviallySmall and
           xys[hi].x - xys[j].x  > triviallySmall   
      then xpect = (n1*ent(lhs) + n2*ent(rhs)) / (n1+n2)
           if xpect < div then  -- cutting here simplifies things
             cut, div = j, xpect end end end 
  end -- end if
  if   cut 
  then b4 = _argmin(lo,   cut, xys,triviallySmall,enoughItems,b4,at,name,out)
       b4 = _argmin(cut+1,hi , xys,triviallySmall,enoughItems,b4,at,name,out)
  else -- if no cut then the original div was never updates and is still correct
       b4 = push(out,  bin.new(#out+1,at,name,b4,xys[hi].x, hi-lo+1,div)).hi end
  return b4 end

return bin
