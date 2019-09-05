
require "lib"

Account = Thing:new{balance = 0}

function Account:deposit (v)
	self.balance = self.balance + v
end

function Account:withdraw (v)
	if v > self.balance then error"insufficient funds" end
	self.balance = self.balance - v
end



