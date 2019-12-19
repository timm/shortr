-- from https://luapower.com/heap
-- 
-- Package:	heap
-- Pkg type:	Lua
-- Version:	12c41d4
-- Author:	Cosmin Apreutesei
-- License:	Public Domain
-- Requires: none
-- Required by:  none

local floor=math.floor

local function heap(add, remove, swap, length, cmp)

	local function moveup(child)
		local parent = floor(child / 2)
		while child > 1 and cmp(child, parent) do
			swap(child, parent)
			child = parent
			parent = floor(child / 2)
		end
		return child
	end

	local function movedown(parent)
		local last = length()
		local child = parent * 2
		while child <= last do
			if child + 1 <= last and cmp(child + 1, child) then
				child = child + 1 --sibling is smaller
			end
			if not cmp(child, parent) then break end
			swap(parent, child)
			parent = child
			child = parent * 2
		end
		return parent
	end

	local function push(...)
		add(...)
		return moveup(length())
	end

	local function pop(i)
		swap(i, length())
		remove()
		movedown(i)
	end

	local function rebalance(i)
		if moveup(i) == i then
			movedown(i)
		end
	end

	return push, pop, rebalance
end


return function(h)
	h = h or {}
	local t, n = h, #h
	local function add(v) n=n+1; t[n]=v end
	local function rem() t[n]=nil; n=n-1 end
	local function swap(i, j) t[i], t[j] = t[j], t[i] end
	local function length() return n end
	local cmp = h.cmp and
		function(i, j) return h.cmp(t[i], t[j]) end or
		function(i, j) return t[i] < t[j] end
	local push, pop, rebalance = heap(add, rem, swap, length, cmp)

	local function get(i)
		assert(i >= 1 and i <= n, 'invalid index')
		return t[i]
	end
	function h:push(v)
		assert(v ~= nil, 'invalid value')
		push(v)
	end
	function h:pop(i)
		assert(n > 0, 'buffer underflow')
		local v = get(i or 1)
		pop(i or 1)
		return v
	end
	function h:peek(i)
		return get(i or 1)
	end
	function h:replace(i, v)
		assert(i >= 1 and i <= n, 'invalid index')
		t[i] = v
		rebalance(i)
	end
	h.length = length

	return h
end

