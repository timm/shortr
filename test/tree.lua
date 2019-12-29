-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

package.path = '../src/?.lua;' .. package.path

local within=require("lib").within
local Lib=require("lib")
local Tbl=require("tbl")
local o,same,map = Lib.o, Lib.same, Lib.map
local ok=require("ok")
local Tree=require("tree")

local function tree1(      t)
  t = Tbl.new{file="../data/weather.csv"}
  Tree.show(Tree.classify(t))
end

ok{tree1=tree1}

function auto(      t)
  t = Tbl.new{file="../data/autohalf.csv"}
  Tree.show(Tree.regression(t))
end

auto()
--`ok{auto=auto}

--ok{tree1=tree1}


