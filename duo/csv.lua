-- vim: ts=2 sw=2 sts=2 et:cindent:
--------- --------- --------- --------- --------- ---------

local function usable(a,    b,todo)
  b,todo = {},{}
  for k,v in pairs(a) do
    if not v:match('%?') then
      b[#b + 1] = k
      todo[#todo+1]=v end end
  return b,todo
end

local function use(a,todo,     b)
  b = {}
  for _,v in pairs(todo) do b[#b+1] = a[v] end
  return b
end

local function atoms(str,  a)
  local function trim(s) return s:gsub("^%s*(.-)%s*$","%1") end
  local function atom(s) return tonumber(s) or s end
  a = {}
  for x in str:gmatch('([^,]+)') do a[#a+1] = atom(trim(x)) end
  return a
end

return function(file,      todo,line,first)
  if file then io.input(file) else stream=io.input() end
  first, line =  true, io.read()
  return function (    a)
    if line then
      a = atoms(line)
      if first then todo,a = usable(a) else a = use(a,todo) end
      first,line = false,io.read()
    else
      if stream then io.close(stream) end end
    return a
  end
end
