-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
------- --------- --------- --------- --------- --------- 

-- A super class for [Num](num.html) and [Sym](sym.html)
-- and [Some](some.html) classes.

require "lib"

local Object=require("object")
local Column={is="Column"}

function Column.new(t)
  local i = Object.new()
  i.me = Column
  i.n   = 0
  t     = t or {}
  i.txt = t.txt or ""
  i.pos = t.pos or 0
  i.w   = i.txt:match("<") and -1 or 1
  i.key = t.key or function (z) return z end
  return i
end

-- ----------
-- And finally...

return Column
