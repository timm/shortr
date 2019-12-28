-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path

local within=require("lib").within
local Lib=require("lib")
local Tbl=require("tbl")
local o,same = Lib.o, Lib.same
local ok=require("ok")
local Tree=require("tree")

local function tree1(      t,n)
  t = Tbl.new{file="../data/weather.csv"}
  Tree.show(Tree.classify(t))
end

tree1()
--ok{tree1=tree1}


