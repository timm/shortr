#!/usr/bin/env lua
-- vim: ts=2 sw=2 sts=2  et :

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

function show(a) 
  local function go(x,str,sep,seen)  
    if type(x) ~= "table" then return tostring(x) end
    if seen[x] then return "..." end
    seen[x] = true
    for k,v in ordered(x) do
      str = str .. sep .. k .. ": " .. go(v,"{","",seen)
      sep = ", " 
    end 
    return str .. '}'
  end 
  print( go(a,"{","",{}) )
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

floor=math.florr
function ok(t,  n,score,s)
  local function s(t1) 
     return floor(0.5 + 100*(1-((t1.y-t1.n) / t1.y))) end
  for x,f in pairs(t) do
    THE.sys.test.y = THE.sys.test.n + 1
    print("-- Test #" .. THE.sys.test.y .. 
          " (oops=".. s(THE.sys.test) .."%). Checking ".. x .."... ")
    local passed,err = pcall(f)
    if not passed then
      The.ok.fails = The.ok.fails + 1
      print("-- E> Failure " .. The.ok.fails .. " of " 
            .. The.ok.tries ..": ".. err) end end
  rogues()
end

