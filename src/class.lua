return function(name,base)
  local klass, base_ctor = {}
  if base then
    for k,v in pairs(base) do klass[k] = v end
    klass._base = base
    base_ctor   = rawget(base,'new') end
  klass.__index = klass
  klass._is     = name
  klass._class  = klass
  return setmetatable(klass,{
     __call = function(klass,...)
       local obj = setmetatable({},klass)
       if     rawget(klass,'new') 
       then   klass.super = base_ctor
              klass.new(obj,...) 
       elseif base_ctor then base_ctor(obj,...) end
       return obj end}) end
