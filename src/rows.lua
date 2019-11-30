-- vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro 
--------- --------- --------- --------- --------- --------- 

require "num"
require "sym"

function 
function data()
  return {w={}, syms={}, nums={}, class=nil, 
          rows={}, name= {}, col={}, _use={}} 
end

function indep(t,c) return not t.w[c] and t.class ~= c end
function dep(t,c)   return not indep(t,c) end

function header(cells,t,       c,w)
  t = t or data()
  t.indeps = {}
  for c0,x in pairs(cells) do
    if not x:match("%?")  then
      c = #t._use+1
      t._use[c] = c0
      t.name[c] = x
      t.col[x]  = c
      if x:match("[<>%$]") 
     then t.nums[c] = num() 
     else t.syms[c] = sym() 
      end 
      if     x:match("<") then t.w[c]  = -1 
      elseif x:match(">") then t.w[c]  =  1  
      elseif x:match("!") then t.class =  c 
      else   t.indeps[ #t.indeps+1 ] = c end end end
  return t
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
