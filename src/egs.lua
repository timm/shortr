-- egs.lua : example usage of the ego.lua
-- (c) 2022 Tim Menzies.  Usage of the works is permitted provided that this
-- instrument is retained with the works, so that any entity that uses the works
-- is notified of this instrument. DISCLAIMER:THE WORKS ARE WITHOUT WARRANTY.  

--------------------------------------------------------------------------------
etc=require"etc"
local big,cli,csv,fmt,is,lt,map= etc.big,etc.cli, etc.csv,etc.etc.fmt
local is,lt,map,o,o,push       = etc.is,etc.lt, etc.map,etc.o, etc.oo,etc.push
local splice,sort,string2thing = etc.splice, etc.sort, etc.string2thing

ego= require"ego"
the = cli(the)
math.random(the.seed or 10019)
local x=egs()
for i=1,5 do oo(x.rows[i]) end; print""
for i=#x.rows-5,#x.rows do oo(x.rows[i]) end
for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end


