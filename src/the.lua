return require"lib".settings[[

brknbad: explore the world better, explore the world for good.
(c) 2022, Tim Menzies

     .-------.  
     | Ba    | Bad <----.  planning= (better - bad)
     |    56 |          |  monitor = (bad - better)
     .-------.------.   |  
             | Be   |   v  
             |    4 | Better  
             .------.  

USAGE:
  ./bnb [OPTIONS]

OPTIONS:
  -bins   -b  max. number of bins            = 16
  -best   -B  best set                       = .5
  -cohen  -c  cohen                          = .35
  -far    -F  how far to go for far          = .9
  -goal   -g  goal                           = recurrence-events
  -K      -K  manage low class counts        = 1
  -leaves -l  number of items in leaves      = .5
  -M      -M  manage low evidence counts     = 2
  -p      -p  coefficient on distance        = 2
  -rule   -r  rule for assessing bins;       = optimize
              one of: (optimize,monitor,tabu)
  -rest   -R  rest is -R*best                = 4
  -some   -s  sample size for distances      = 512
  -seed   -S  seed                           = 10019
  -wait   -w  wait                           = 10

OPTIONS (other):
  -dump  -d   dump stack on error then quit = false
  -file  -f   file name        = ../etc/data/breastcancer.csv
  -help  -h   show help        = false
  -todo  -t   start up action  = nothing
]]
