----------------------
-- Showing off Colon mode.
-- module: colon

--- first useless function.
-- Optional type specifiers are allowed in this format
-- treturn: ?table|string
function a(x,  -- num: sdas
           num) -- nu: asda
  return x end

--- implicit table can always use colon notation.
person2 = {
    id=true, -- string: official ID number
    sex=true,    -- string: one of 'M', 'F' or 'N'
     spouse=true, -- ?person3: wife or husband
    }

--- another hing
-- module: fred

--- first useless function.
-- Optional type specifiers are allowed in this format
-- treturn: ?table|string
function ba(x,  -- num: sdas
           num) -- nu: asda
  return x end

--- implicit table can always use colon notation.
person32 = {
    id=true, -- string: official ID number
    sex=true,    -- string: one of 'M', 'F' or 'N'
     spouse=true, -- ?person3: wife or husband
    }


