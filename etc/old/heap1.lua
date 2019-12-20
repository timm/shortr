-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

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

local function heap0(add, remove, swap, length, cmp)
	local function moveup(kid)
		local mum = floor(kid / 2)
		while kid > 1 and cmp(kid, mum) do
			swap(kid, mum)
			kid = mum
			mum = floor(kid / 2)
		end
		return kid
	end

	local function movedown(mum)
		local last = length()
		local kid = mum * 2
		while kid <= last do
			if kid + 1 <= last and cmp(kid + 1, kid) then
				kid = kid + 1 --sibling is smaller
			end
			if not cmp(kid, mum) then break end
			swap(mum, kid)
			mum = kid
			kid = mum * 2
		end
		return mum
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
		if moveup(i) == i then movedown(i) end
	end

	return push, pop, rebalance
end


local  function heap(h)
	h = h or {}
	local t, n = h, #h
	local function add(v) n=n+1; t[n]=v end
	local function rem() t[n]=nil; n=n-1 end
	local function swap(i, j) t[i], t[j] = t[j], t[i] end
	local function length() return n end
	local cmp = h.cmp and
		function(i, j) return h.cmp(t[i], t[j]) end or
		function(i, j) return t[i] < t[j] end
	local push, pop, rebalance = heap0(add, rem, swap, length, cmp)
	local function get(i) return t[i] end
	function h:peek(i) return get(i or 1) end
	function h:push(v) push(v) end
	function h:pop(i)
		local v = get(i or 1)
		pop(i or 1)
		return v
	end
	function h:replace(i, v)
		t[i] = v
		rebalance(i)
	end
	h.length = length
  function h:has() return t end
	return h
end

return heap
