
require "obj"

SpecialAccount = Account:new()

s = SpecialAccount:new{limit=1000.00}
s:deposit(100.00)

function SpecialAccount:withdraw (v)
	if v - self.balance >= self:getLimit() then
		error "insufficient funds"
	end
	self.balance = self.balance - v
end

function SpecialAccount:getLimit ()
	return self.limit or 0
end

s:withdraw(10)
print(s)
