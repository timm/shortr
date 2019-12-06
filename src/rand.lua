-- vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro 
--------- --------- --------- --------- --------- --------- 

-- <img 
--  src="https://github.com/timm/lua/raw/master/etc/img/random.png">

-- Lua's math.random() is an interface to the C rand() function
-- provided by the OS libc; its implementation varies by platform
-- so the numbers it generates also varies. 
-- 
-- Can't have that (tests won't port across platforms)
-- 
-- The following is library is recommended as a
-- replacement for the ANSI C rand() and srand()
-- functions, particularly in simulation applications
-- where the statistical 'goodness' of the random
-- number generator is important. 
-- 
-- The generator used in this library is a so-called
-- 'Lehmer random number generator' which returns a
-- pseudo-random number uniformly distributed 0.0 and
-- 1.0. The period is (m - 1) where m=2,147,483,647
-- and the smallest and largest possible values are (1/m) 
-- and 1-(1/m) respectively. For more details
-- see:
-- 
-- - "Random Number Generators: Good Ones Are Hard To Find"
--    Steve Park and Keith Miller
--    Communications of the ACM, October 1988
--
-- Also, the raw random number generator is wrapped in a
-- "97 table" to increase randomness.

local Rand = {}

local seed0 = 10013
local seed  = seed0
local multipler = 16807.0
local modulus = 2147483647.0
local randomtable = nil

local function park_miller_randomizer()
  seed = (multipler * seed) % modulus -- cycle= 2,147,483,646 numbers
  return seed / modulus end 

-- Reset the random seed.
function Rand.seed(n) 
  seed = n and n or seed0; randomtable = nil end

-- Pull the next random number.
function Rand.r(  x,i)
  if randomtable == nil then
    randomtable = {}
    for i = 1, 97 do
     randomtable[i] = park_miller_randomizer() end end
  x = park_miller_randomizer()
  i = 1 + math.floor(97*x)
  x, randomtable[i] = randomtable[i], x
  return x
end

-- -----------
-- And finally...
return Rand
