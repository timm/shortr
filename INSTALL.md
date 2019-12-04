# INSTALL


<img align=right  width=300
     src="https://github.com/timm/lua/raw/master/etc/img/install.png">


- [Install LUA](https://www.lua.org/start.html#installing)
- Check out this repo, 
  - e.g. to  `/Users/timm/gits/lua`.
- Every file in src/*.lua and test/*.lua knows its own dependancies
  so you can run any of those by:
  - changing to those directories
     - and running `lua x.lua`
     - or, in a lua session in that directory `local Num=require('num')`.
- Or, you can access this code by placing it at the end of  `LUA_PATH`; e.g
  - `export LUA_PATH="$LUA_PATH:/Users/timm/gits/lua/src/?.lua"`
  - Once you do that, you can load this code via; e.g.
    - `local Num=require('num')`

