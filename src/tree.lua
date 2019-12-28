-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 
 
local THE  = require("the")
local Lib  = require("lib")
local Some = require("some")
local Num  = require("num")
local divs= require("divs")
local Tree = {is="Tree"}
local sprintf, has, same = Lib.sprintf, Lib.has, Lib.same

-- Operates over `splits`. `Num`eric and `Sym`bolic
-- columns are divided into lists of the following form:

--      {fx,   --  a function that finds a value
--       _all, -- a list  or rows in this split      
--       use,  -- returns true for rows in this split
--       show, -- a print string from this split        
--       stats -- a summary of yvalues in this split
--       }

local function split(cols,rows, my)
  -- Split a numeric column.
  local function splitNum(col) return divs.some(rows,my) end

  -- For symbolic columns, report the variability of the
  -- y value associated with each symbolic value.
  local function splitSym(col)
    local splits={}
    for _,row in pairs(rows) do
      local x = my.fx(row)
      if x ~= THE.char.skip then
        splits[x] = splits[x] or {
                        fx=my.fx, lo=x, hi=x, 
                        _all=  {}, 
                        use =  function(z) return my.fx(z)==x end,
                        show=  string.format("%s",x),
                        stats= my.ytype.new{key=my.fy}} 
        my.ytype.add(splits[x].stats, row)
        push(splits[x]._all,row) end end
    return splits
  end
 
  -- Report the splits for this `col` as well as the
  -- expected value of the variability after the split.
  local function split(col)
    my.fx = function(row) return row.cells[col.pos] end
    local f = col.me==Sym and splitSym or splitNum
    local splits= f(col)
    local n,xpect=0,0
    for _,split1 in pairs(splits) do
      xpect = xpect + 
              split1.stats.n * my.ytype.var(split1.stats)
      n    = n + split1.stats.n
    end
    return xpect/n, splits
  end
 
  -- Return the splits that most reduce variability's expected value 
  local best,col,splits
  for _,col1 in pairs(cols) do
    local xpect, splits1 = split(col1)
    if not col then
      best,col,splits = xpect,col1,splits1
    else
      if xpect*my.trvial < best then
        best,col,splits = xpect, col1,splits1 end end end
  return col,splits
end

function Tree.new(my,cols,rows,lvl,up)
  my = has(my)(THE.tree){fx=same, fy=same,ytype=Num}
  local i = Object.new()
  i.me  = Tree
  i.stats = up and up.stats or nil
  i._up =
  lvl     = lvl or 0
  i._up   = up
  if  lvl  >  my.depth  then return i end
  if  #lst <= my.minObs then return i end
  local col,splits = Tree.split(cols, rows, my)
  if #splits == 1 then return i end
  for _,split in pairs(splits) do
    i.kids= push(i.kids, {use= split.use,
                          sub= Tree.new(my,cols,
                                        split._all,
                                        split.stats,
                                        lvl+1, i)}) 
  end 
end

function Tree.show(i,pre)
  pre = pre or ""
  printf("%s: %s", pre,i.here.n)
  if   #i.kids == 0 
  then print(i.me.mid(i.here), i.me.var(i.here))
  else print("")
  end   
  for _,one in pairs(i.kids) do 
    pre=pre .. "|   "
    Tree.show(k,pre) end
end

return Tree
