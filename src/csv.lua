-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- ---------

-- <img align=right width=200 
--  src="https://github.com/timm/lua/raw/master/etc/img/csv.png">
-- Returns a function that reads a csv file.  Coerce string 
-- to numbers (or strings) as appropriate. If a line one 
-- word holds the skip character `?`, then ignore that column.

-- ---------
-- The `usable` function works out what columns to use and
-- `use` actually does the selection.

require "lib"

local function usable(a,    b,todo)
  b,todo = {},{}
  for k,v in pairs(a) do
    if not v:match('%?') then 
      b[#b + 1] = k; todo[#todo+1]=v end end 
  return b,todo
end

local function use(a,todo,     b)
  b = {}
  for _,v in pairs(todo) do b[#b+1] = a[v] end 
  return b
end

-- -----------------
-- Split a line on comma, trim leading/training whitespace, 
-- coerce strings to numbers (or strings) as appropriate.

local function atoms(str,  a)
  local function trim(s) return s:gsub("^%s*(.-)%s*$","%1") end
  local function atom(s) return tostring(s) or s end
  a = {}
  for x in str:gmatch('([^,]+)') do a[#a+1] = atom(trim(x)) end
  return a
end

-- ----------
-- Run the rows, call the 
--  and `f` on all the other rows.
-- If `file` is nil then read from standard input.

return function (file, f,    a,b4,todo)
  if file then io.input(file) else stream=io.input() end
  f  = f or print
  b4, line =  true, io.read()
  while line do
    a = atoms(line)
    if b4 then todo,a = usable(a) else a = use(a,todo) end
    f(a)
    b4, line = false, io.read()
  end
  if stream then io.close(stream) end
end
