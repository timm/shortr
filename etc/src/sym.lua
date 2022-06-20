-- SHORTR.lua (c) 2022, Tim Menzies <timm@ieee.org>, BSD2 license.
-- Semi-supervised multi-objective optimization XAI. From N
-- items, find and explain the best ones, using just log(N) 
-- evals.  All in a few hundreds lines of LUA.

--                                        ___                        
--                                       /\_ \                       
--   ____  __  __    ___ ___             \//\ \    __  __     __     
--  /',__\/\ \/\ \ /' __` __`\             \ \ \  /\ \/\ \  /'__`\   
-- /\__, `\ \ \_\ \/\ \/\ \/\ \      __     \_\ \_\ \ \_\ \/\ \L\.\_ 
-- \/\____/\/`____ \ \_\ \_\ \_\    /\_\    /\____\\ \____/\ \__/.\_\
--  \/___/  `/___/> \/_/\/_/\/_/    \/_/    \/____/ \/___/  \/__/\/_/
--             /\___/                                                
--             \/__/                                                 

local  _  = require"lib"
local obj = _.obj
