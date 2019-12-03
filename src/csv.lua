-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- ---------

-- <img align=right 
--  src="https://github.com/timm/lua/raw/master/etc/img/csv.png">
-- Returns a function that reads a csv file.  Coerce string 
-- to numbers (or strings) as appropriate. If a line one 
-- word holds the skip character `?`, then ignore that column.

-- ---------
-- The `usable` function works out what columns to use and
-- `use` actually does the selection.

local function usable(a,    b,c)
  b,c = {},{}
  for k,v in pairs(a) do
    if not v:match('%?') then b[#b + 1] = k; c[#c+1]=v end end 
  return b,c
end

local function use(a,u,     b)
  b = {}
  for _,v in pairs(u) do b[#b+1] = a[v] end 
  return b
end

-- -----------------
-- Split a line on comma, trim leading/training whitespace, 
-- coerce strings to numbers (or strings) as appropriate.

local function split(str,  a)
  local function trim(s) return s:gsub("^%s*(.-)%s*$","%1") end
  local function atom(s) return tostring(s) or s end
  a = {}
  for x in str:gmatch('([^,]+)') do a[#a+1] = atom(trim(x)) end
  return a
end

-- ----------
-- Run the rows, call the `f0` function on
--  line one and `f` on all the other rows.

return function (file, f0, f,    first,todo,cells)
  if file then io.input(file) else stream=io.input() end
  first, line =  true, io.read()
  while line do
    if first then 
      todo,cells = usable( split(line) )
      f0(cells)  
    else 
      f( use( split(line), todo) ) 
    end
    first, line = false, io.read()
  end
  if stream then io.close(stream) end
end
