-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

local Rand = require("rand")

local y,n = 0,0

local function rogues(    skip)
  skip = {
    jit=true, utf8=true, math=true, package=true, table=true,
    coroutine=true, bit=true, os=true, io=true, bit32=true,
    string=true, arg=true, debug=true, _VERSION=true, _G=true,
    getmetatable=true, print=true, rawequal=true, dofile=true,
    load=true, collectgarbage=true, rawget=true, loadfile=true,
    tostring=true, pairs=true, pcall=true, error=true,
    xpcall=true, select=true, assert=true, rawset=true,
    setmetatable=true, type=true, rawlen=true, next=true,
    ipairs=true, require=true, tonumber=true}
  for k,v in pairs( _G ) do
    if not skip[k] then
      if k:match("^[^A-Z]") then
        print("-- rogue ["..k.."]") end end end
end

local function s() 
  return math.floor(0.5+100*(1-((y-n)/y))) end

return function(t,  score)
  for x,f in pairs(t) do
    y = y + 1
    print("-- Test #" .. y ..
          " (oops= ".. n .. " =" .. s() .."%) : " .. x )
    Rand.seed()
    local passed,err = pcall(f)
    if not passed then
      n = n + 1
      print("-- E> Failure " .. n .. " of "
            .. y ..": ".. err) end end
  rogues()
end
