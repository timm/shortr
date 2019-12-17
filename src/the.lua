-- vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro 
--------- --------- --------- --------- --------- --------- 

return {
  dist = { p      = 2      },
  char = { skip   = "?"    },
  rand = { seed    = 10013 },
  some = { most    = 128   },
  divs = { step    = 0.5    ,
           most    = 128    ,
           skip    = "?"    ,
           depth   = 1000   ,
           cohen   = 0.3    ,
           trivial = 1.05  }
}
