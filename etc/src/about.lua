-- ## Options
       
-- To process this string, you'll need two commands from
-- `lib`: `opts` and `cli`.
--
-- To extract keys and defaults for the settings,  use   
-- `the = opts(require"about))`.
-- 
-- To update the defaults from any command-line flags, use   
-- `the = cli(opts(require"about"))`.
return [[
  
TRICKS : bag of LUA tricks, (c) 2022 Tim Menzies, BSD2
  
OPTIONS:
  -s  --seed    random number seed = 10019
  
OPTIONS (other):
  -f  --file    data file        = ../../data/auto93.csv
  -g  --go      start up action  =  nothing,
  -h  --help    show help        =  false]]

