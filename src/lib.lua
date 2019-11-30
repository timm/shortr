--vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro 
--------- --------- --------- --------- --------- --------- 

local THE=require("the").misc

math.randomseed(THE.seed)
r=math.random

function within(a,b,c) return b>=a and b<=c end

function round(x) return math.floor( x + 0.5 ) end

function o(t) print(oo(t))  end

function oo(t,     s,sep)
  s, sep = '',''
  for k, v in pairs(t) do
    if type(v) ~= 'function' then
      if not (type(k)=='string' and k:match("^_")) then
        v   = type(v) == 'table' and oo(v) or tostring(v) 
        s   = s .. sep .. tostring(k) .. '=' .. v
        sep = ', ' end end end
  return '{' .. s .. '}'
end


