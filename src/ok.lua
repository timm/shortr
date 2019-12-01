-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

local seed=require("the").misc.seed

local y,n = 0,0

local function rogues(    skip)
  skip = {jit=true, utf8=true, math=true, package=true,
            table=true, coroutine=true, bit=true, os=true,
            io=true, bit32=true, string=true, arg=true,
            debug=true, _VERSION=true, _G=true }
  for k,v in pairs( _G ) do
    if type(v) ~= "function" and not skip[k] then
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
    math.randomseed(seed)
    local passed,err = pcall(f)
    if not passed then
      n = n + 1
      print("-- E> Failure " .. n .. " of "
            .. y ..": ".. err) end end
  rogues()
end
