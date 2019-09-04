#!/usr/bin/env lua
-- vim: ts=2 sw=2 sts=2  et :

--[[

# Never seen it brighter 

aaaj how about those 

--]]

require "the"

function inc(a,i,    new) 
  new  = (a[i] or 0) + 1 
  a[i] = new
  return new
end

function ordered(t)
  local i,tmp = 0,{}
  for key,_ in pairs(t) do tmp[#tmp+1] = key end
  table.sort(tmp)
  return function () 
    if i < #tmp then 
      i = i+1
      return tmp[i], t[tmp[i]] end end 
end

function rogues(    ignore,match)
  ignore = {
    jit=true, utf8=true,math=true, package=true, table=true, 
    coroutine=true, bit=true, os=true, io=true, 
    bit32=true, string=true, arg=true, debug=true, 
    _VERSION=true, _G=true }
  for k,v in pairs( _G ) do
    if type(v) ~= "function" and not ignore[k] then
       if k:match("^[^A-Z]") then
         print("-- warning, rogue local ["..k.."]") 
  end end end 
end 

floor=math.floor
function ok(t,     n,score,s,my)
  my=THE.sys.ok
  local function s(t1)
     return floor(0.5 + 100*(1-(
             (my.tries - my.fails) / my.tries))) end
  for x,f in pairs(t) do
    my.tries = my.tries + 1
    print("-- Test #" .. my.tries .. 
          " (oops=".. s() .."%). Checking ".. x .." ... ")
    local passed,err = pcall(f)
    if not passed then
      my.fails = my.fails + 1
      print("-- E> Failure " .. my.fails .. " of " 
            .. my.tries ..": ".. err) end end
  rogues()
end

do
  local ids=0
  local function id()
    ids = ids + 1
    return n
  end
end

Object={}
function Object:new(o)
  o = o or {}   -- create object if user does not provide one
  setmetatable(o, self)
  self.id  = self.id or  id()
  self.__index = self
  return o
end

--[[

## System stuff

Refine `require` (so it makes globals when appropriate) and `tostring`
(so it can print tables).

--]]

_require = require
function require(x,    new) 
  new = _require(x)
  if type(new)=="table" then
    if new[x] then
      _ENV[x] = new[x] end end
  return new
end

_tostring=tostring
function tostring(a) 
  local function go(x,str,sep,seen)  
    if type(x) ~= "table" then return _tostring(x) end
    if seen[x] then return "..." end
    seen[x] = true
    for k,v in ordered(x) do
      str = str .. sep .. k .. ": " .. go(v,"{","",seen)
      sep = ", " 
    end 
    return str .. '}'
  end 
  return go(a,"{","",{}) 
end  

