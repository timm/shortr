-- class.lua
-- Compatible with Lua 5.1 (not 5.0).
function class(base, init)
   local c = {}    -- a new class instance
   if not init and type(base) == 'function' then
      init = base
      base = nil
   elseif type(base) == 'table' then
    -- our new class is a shallow copy of the base class!
      for i,v in pairs(base) do
         c[i] = v
      end
      c._base = base
   end
   -- the class will be the metatable for all its objects,
   -- and they will look up their methods in it.
   c.__index = c

   -- expose a constructor which can be called by <classname>(<args>)
   local mt = {}
   mt.__call = function(class_tbl, ...)
   local obj = {}
   setmetatable(obj,c)
   if init then
      init(obj,...)
   else 
      -- make sure that any stuff from the base class is initialized!
      if base and base.init then
      base.init(obj, ...)
      end
   end
   return obj
   end
   c.init = init
   c.is_a = function(self, klass)
      local m = getmetatable(self)
      while m do 
         if m == klass then return true end
         m = m._base
      end
      return false
   end
   setmetatable(c, mt)
   return c
end


If this change is made:
--- class_orig.lua      2009-07-24 20:53:25.218750000 -0400
+++ class.lua   2009-07-24 20:53:49.734375000 -0400
@@ -21,8 +21,8 @@
   mt.__call = function(class_tbl,...)
     local obj = {}
     setmetatable(obj,c)
-    if ctor then
-       ctor(obj,...)
+    if class_tbl.init then
+       class_tbl.init(obj,...)
     else
     -- make sure that any stuff from the base class is initialized!
        if base and base.init then
then we alternately may declare classes in this way:
A = class()
function A:init(x)
  self.x = x
end
function A:test()
  print(self.x)
end

B = class(A)
function B:init(x,y)
  A.init(self,x)
  self.y = y
end
BTW, you may note that class.lua also works for operators:
function A:__add(b)
  return A(self.x + b.x)
end
