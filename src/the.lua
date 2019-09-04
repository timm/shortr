#!/usr/bin/env lua
-- vim: ts=2 sw=2 sts=2  et :

local function the0() return {
  a=   {b= 0},
  sys= {test= {tries= 0, failes= 0}}
  }
end

function the(  y,n)
  if   _G["THE"] 
  then y,n = THE.sys.test.tries, THE.sys.test.fails 
  else y,n = 0,0 
  end
  THE = the0()
  THE.sys.test.tries, THE.sys.test.fails = y,n
end

THE = the()
