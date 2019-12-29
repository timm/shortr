-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 
 
local THE  = require("the")
local Lib  = require("lib")
local Object = require("object")
local Sym  = require("sym")
local Num  = require("num")
local divs = require("divs2")
local Tree = {is="Tree"}
local o,has,same,push = Lib.o,Lib.has,Lib.same,Lib.push
local map,printf = Lib.map, Lib.printf
local splitter

-- Operates over `splits`. `Num`eric and `Sym`bolic
-- columns are divided into lists of the following form:

--      {has,  -- A list  of rows in this split.
--             -- This list will get converted to a tree.
--       use,  -- returns true for rows in this split
--       show, -- a print string from this split        
--       stats -- a summary of yvalues in this split
--       }

-- -----
-- `Classification` and `regression` trees are just
-- different ways to `grow` a tree.
function Tree.regression(t) return Tree.grow(t, Num) end
function Tree.classify(t)   return Tree.grow(t, Sym) end

-- To `grow` a tree, create a `new` root node.
function Tree.grow(t,ytype,fy)
  fy = fy or function(r) return r.cells[t.cols.y.klass.pos] end
  return Tree.new( {ytype=ytype, fy=fy},
                    t.cols.x.all, t.rows,t.cols.y.klass)
end

-- -----
-- `New` root notes create sub-trees
--  as a side-effect of initialization (using the `split`
-- function).
function Tree.new(my,cols,rows,stats,lvl,up)
  lvl = lvl or 0
  my = has(my)(THE.tree){fx=same, fy=same, ytype=Num}
  local i = Object.new()
  i.me    = Tree
  i.stats = stats
  i._up   = up
  i.lvl   = lvl 
  if  i.lvl  >  my.depth  then return i end
  if  #rows <= my.minObs then return i end
  local col,splits,all = splitter(cols, rows, my)
  if #splits == 1 then return i end
  i.kids = splits
  for _,kid in pairs(i.kids) do
    kid.has = Tree.new(my,cols,kid.has,kid.stats, lvl+1,i) 
    kid.what = col.txt
  end 
  return i
end

-- -----
-- `Split` know how to divide numeric and symbolic
-- columns, then find the best split over all columns.
function splitter(cols,rows, my)
  -- Split a numeric column.
  local function splitNum(col) 
     return  divs.some(rows,my) end

  -- For symbolic columns, report the variability of the
  -- y value associated with each symbolic value.
  local function splitSym(col)
    local splits={}
    for _,row in pairs(rows) do
      local f = my.fx
      local x = f(row)
      if x ~= THE.char.skip then
        splits[x] = splits[x] or {
                        lo=x, hi=x, 
                        has=  {}, 
                        use =  function(z) return f(z)==x end,
                        show=  string.format("%s",x),
                        stats= my.ytype.new{key=my.fy}} 
        my.ytype.add(splits[x].stats, row)
        push(splits[x].has,row) end end
    return splits
  end
 
  -- Report the splits for this `col` as well as the
  -- expected value of the variability after the split.
  local function splitter1(col)
    my.fx = function(row) return row.cells[col.pos] end
    local f = col.me==Sym and splitSym or splitNum
    local splits= f(col)
    local xpect,n = 0, 0.0000000001
    for _,split1 in pairs(splits) do
      n = n + split1.stats.n
      xpect = xpect + 
              split1.stats.n * my.ytype.var(split1.stats)
    end
    return xpect/n, splits
  end
 
  -- Return the splits that most reduce variability's expected value 
  local min,col,splits
  local all={}
  for _,col1 in pairs(cols) do
    local xpect, splits1 = splitter1(col1)
    all[col1] = xpect -- hook 4 future work (feature selection)
    if not col then
      min,col,splits = xpect,col1,splits1
    else
      if xpect*my.trivial < min then
        min,col,splits = xpect, col1,splits1 end end end
  return col,splits,all
end

-- -----
-- Print a tree
function Tree.show(i,lvl)
  lvl = lvl or 0
  pre = string.rep("|  ",lvl)
  for _,kid in pairs(i.kids or {}) do 
    print(pre.. kid.what .. " " .. kid.show) 
    Tree.show(kid.has,lvl+1) end
end

-- -----
-- And finally...
return Tree

