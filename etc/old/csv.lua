-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- ---------

do
  local KLASS= "[=<>]" -- symbols that mark a class name
  local SEP  = ","     -- row field seperator
  --------------------------------------------------
  local function about(str, nx, ny, out)
    if string.find(str,KLASS) ~= nil then
       ny = ny+1; out.pos = ny; out.xy = "y"
     else
       nx = nx+1; out.pos = nx; out.xy = "x"
     end
     return out, nx, ny
  end 
  --------------------------------------------------
  function xys()
    local names, abouts =  {}, {}
    local row, line     = -1, io.read()
    return function ()
      while line ~= nil do
        local xy = {x= {}, y={}}
        local col, nx, ny = 0, 0, 0
        for z in string.gmatch(line, "([^".. SEP .."]+)" ) do
           col = col + 1
           if row < 0 then
             abouts[col], nx, ny = about(z,nx,ny,{})
           end
           local a  = abouts[col]
           xy[a.xy][a.pos] = tonumber(z) or z
        end
        row, line = row + 1, io.read()
        if row == 0 then
          names = xy
        else
          return row, names, xy
        end end
      return nil end 
  end
end
