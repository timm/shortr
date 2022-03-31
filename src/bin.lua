local the,_,SYM = require"the", require"lib", require"sym"
local fmt,per,upx,push,sort = _.fmt,_.per,_.upx,_.push,_.sort
local ent,id = _.ent,_.id

local BIN=obj"BIN"
function BIN.new(mark,at,name,lo,hi,has) 
  return new(BIN, {id=id(), mark=mark,at=at,name=name,
                   lo=lo,hi=hi,ys=ys or SYM()}) end

function BIN:_tostring()
  local x,lo,hi,big = self.name, self.lo, self.hi. math.huge
 if      lo ==  hi  then return fmt("%s == %s",x,lo)  
  elseif hi ==  big then return fmt("%s >= %s",x,lo)  
  elseif lo == -big then return fmt("%s <  %s",x,hi)  
  else                   return fmt("%s <= %s < %s",lo,x,hi) end end

function BIN:select(row)
  local x, lo, hi = row[self.at], self.lo, self.hi
  return x=="?" or lo == hi and lo == x or lo <= x and x < hi end

function BIN:add(x,y)
  if x<self.lo then self.lo = x end 
  if x>self.lo then self.hi = x end 
  ys:add(y) end

---     _  | _  _ _   _ _  _ _|_|_  _  _| _
---    (_  |(_|_\_\  | | |(/_ | | |(_)(_|_\

function BIN.merges(bins)
  local j,n,new = 1,length(bins),{}
  while j <= n do
    a=bins[j]
    if j < n then
      b = bins[j+1]
      if a.hi == b.lo then
        a.hi = b.hi
        a.ys = a.ys:merge(b.ys)
        j    = j + 1 end end
    j=j+1
    push(new,a) end
  return #new < #bins and BIN.merges(new) or bins end

local argmin
function bin.Xys(xys,at,name)
  xys                  = sort(xys, upx)
  local triviallySmall = the.cohen*(per(xys,.9).x - per(xys, .1).x)/2.56 
  local enoughItems    = #xys / the.bins
  local out            = {}
  argmin(1,#xys, xys, triviallySmall, enoughItems, -math.huge, at,name, out)
  out[#out].hi =  math.huge 
  return out end

function argmin(lo, hi, xys, triviallySmall, enoughItems, b4, at, name,out)
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
  then b4 = argmin(lo,   cut, xys,triviallySmall,enoughItems,b4,at,name,out)
       b4 = argmin(cut+1,hi , xys,triviallySmall,enoughItems,b4,at,name,out)
  else -- if no cut then the original div was never updates and is still correct
       b4 = push(out,  bin.new(#out+1,at,name,b4,xys[hi].x, hi-lo+1,div)).hi end
  return b4 end

return bin
