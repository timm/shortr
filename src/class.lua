local function update (t,...)
  for i = 1,select('#',...) do
    for k,v in pairs(select(i,...)) do
      t[k] = v end end
  return t end

local function import(t,...)
  local other
  t = t or _ENV or getfenv(2)
  local libs = {}
  if select('#',...)==0 then -- default is to pull in this library!
    libs[1] = ml
  else
    for i = 1,select('#',...) do
      local lib = select(i,...)
      if type(lib) == 'string' then
        local value = _G[lib]
        if not value then -- lazy require!
          value = require (lib)
          -- and use the module part of package for the key
          lib = lib:match '[%w_]+$' end
        lib = {[lib]=value} end
      libs[i] = lib end end
  return update(t,table.unpack(libs)) end

return function(name,base)
  local klass, base_ctor = {}
  if base then
    --import(klass,base)
    klass._base = base
    base_ctor = rawget(base,'new') end
  klass.__index = klass
  klass._is =name
  klass._class = klass
  return setmetatable(klass,{
     __call = function(klass,...)
       local obj = setmetatable({},klass)
       if rawget(klass,'new') then
         klass.super = base_ctor
         local res = klass.new(obj,...) -- call our constructor
         if res then -- which can return a new self..
           obj = setmetatable(res,klass) end
       elseif base_ctor then -- call base ctor automatically
         base_ctor(obj,...) end
       return obj end }) end
