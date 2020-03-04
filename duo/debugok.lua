local function b(x) return x/y end

local function debug(a)
  return a/b(a)
end

debug(10)
