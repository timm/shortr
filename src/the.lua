return require("tricks").cli[[

lua l5.lua [OPTIONS]
(c) 2022, Tim Menzies, BSD-2-Clause
Explore the world better; explore it for good.

OPTIONS:
  -cohen     -c cohen                   =  .35
  -far       -F how far to seek poles   = .9
  -keep      -k items to keep           = 256
  -K         -K manage low class counts = 1
  -M         -M manage low evidence counts = 2
  -minItems  -m min items in a rang e   = .5
  -p         -p euclidean coefficient   = 2
  -some      -S sample size for rows    = 512

OPTIONS, other:
  -dump      -d stackdump on error      = false
  -file      -f data file               = ../etc/data/auto93.csv
  -help      -h show help               = false
  -rnd       -r round numbers           = %5.2f
  -seed      -s random number seed      = 10019
  -todo      -t start-up action         = nothing
  -n1        -n1 #repeated trials       = 20
  -n2        -n2 samples per trial      = 100
]]
