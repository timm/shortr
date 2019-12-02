-- vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro 
--------- --------- --------- --------- --------- --------- 

local Object = require("object")
local Cols   = require("cols")
local Row    = require("rows")
local Tbl    = {is="Tbl"}

function Tbl.new(f)
  local i = Object.new()
  i._use, i.me   = {}, Tbl
  i.rows, i.cols = {}, Cols.new()
  return i
end

function row(t,cells,     x,r)
  r= #t.rows+1
  t.rows[r] = {}
  for c,c0 in pairs(t._use) do
    x = cells[c0]
    if x ~= "?" then
      if t.nums[c] then 
          x = tonumber(x)
        numInc(t.nums[c], x)
      else
          symInc(t.syms[c], x)
    end end
    t.rows[r][c] = x  end
  return t
end  

function clone(data0, rows,   data1)
   data1 = header(data0.name)
   for _,cells in pairs(rows or data0.rows) do 
     row(data1, cells) end
   return data1
end

-- ## Making `data` from Ram 
--
-- Reading data from disk, is handled by the
-- `rows` function (that sets some defaults), after
-- which time it calls `rows1` to do the actually
-- stream over the disk data. 

function rows1(stream, t,f0,f,   first,line,cells)
  first,line = true,io.read()
  while line do
    line= line:gsub("[\t\r ]*","")
              :gsub("#.*","")
    cells = split(line)
    line = io.read()
    if #cells > 0 then
      if first then f0(cells,t) else f(t,cells) end end
      first = false
  end 
  io.close(stream)
  return t
end

function rows(file,t,f0,f,      stream,txt,cells,r,line)
  return rows1( file and io.input(file) -- reading from some specified file
                      or io.input(),    -- reading from standard input
                t  or data(), f0 or header, f or row) end 
