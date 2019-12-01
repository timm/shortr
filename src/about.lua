-- [home](index.html) :: [about](about.html) :: [github](http://github.com/timm/lua) :: [discuss](http://github.com/timm/lua/issues) :: [&copy; 2020](https://github.com/timm/lua/blob/master/LICENSE.md) by [timm](http://menzies.us)

-- This file shows the conventions used in this code.

-- ## No Globals, all modules

-- All files are modules that usually starts with `local X={}` and ends 
-- with `return X`.
--
-- Whenever my tests are run (see below),
-- the `rogues` function from [ok.lua](ok.html)
-- warns about anything that has inappropriately escaped to the global space.

-- ## Optional Config
--
-- The file [the.lua](the.html) contains defaults that code can include using    
-- 
-- `local THE=require("the")`.

-- ---------------------
-- ## Test-Driven Development
--
-- Most files in `src/X.lua` have unit test files `test/X.lua`. The unit tests files
-- patch the load path then load `src` files. For example, see right.



package.path = '../src/?.lua;' .. package.path
require "lib"
local ok = require("ok")

roguevar=22 -- this rogue global will generate a warning message

ok{notok = function() assert(1~=1,"not eq") end}
ok{ok = function() assert(1==1,"not equal") end}

o{aa=1, bb=2, cc={dd=22, ee=30, ff={10}}}


-- My `ok` test engine (shown at right, defined in [ok.lua](ok.html))
-- accepts pairs `testName=function`, then
-- prints `testName`; then
-- runs the `function`.
--
-- If `function`
-- crashes, `ok` will just print the stack trace without terminating the program
-- (so having crashing code does not block other tests from executing).

-- ---------------------
-- ## Gutless Objects 
--
-- My "object-ish" methods have no colon "`:`" operator.
-- Why kludge objects when Lua has such a clean module system? 
--
-- Gutless supports encapsulation, polymorphism, constructors,
-- inheritance of instance variables (but not inheritance of methods).

-- An example Gutless file is shown at right. 
-- In that example, `Object` and `Column` are classes
-- while `i` is an instance.
-- Note that that file:
-- start by defining a new class `local Column={is="Column"}`
-- and  includes the super class; e.g.    
-- 
-- `local Object=require("Object")`
--
-- The file also needs a constructor of `function Column.new(t)` where the table `t`
-- contains overrides to the defaults.
-- That constructor starts by calling `Object.new()` and
-- also  adds a point `i.me` back to the class.
--
-- This `i.me` pointer enables polymorphism.  To send
-- a `message`  back to an class with arguments `args` (and to
-- select the receiver based on
-- the type of the object), then call:       
--
-- `i.me.message(i, args)`

local Object=require("object")
local Column={is="Column"}

function Column.new(t)
  local i = Object.new() -- begin standard creation stuff
  i.me = Column
  i.n   = 0
  t     = t or {}
  i.txt = t.txt or "" -- begin stuff just for Column instances
  i.pos = t.pos or 0
  i.w   = i.txt:match("<") and -1 or 1
  i.key = t.key or function (z) return z end
  return i -- don't forget to return the new instance.
end

return Column



