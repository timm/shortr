
require "lib"

Account={}

function Account:new(spec)
  local x=Thing.new(self,spec)
  print(spec)
  x.balance=spec.balanace or 100
  x.types={aa={},bb={}}
  return x
end

--Account = Thing:new{balance=0,ccc=2}
--types={aa={1},bb={2}}}


function Account:deposit (v)
	self.balance = self.balance + v
end

function Account:withdraw (v)
	if v > self.balance then error"insufficient funds" end
	self.balance = self.balance - v
end

x=Account:new{balance=2000}
y=Account:new{}

x.types.aa[#x.types.aa+1]="fred"
print(x)
print(y)
