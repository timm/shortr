-- vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro 
--------- --------- --------- --------- --------- --------- 

return {
  dist = { p      = 2      },
  char = { skip   = "?"    },
  rand = { seed    = 10013 },
  some = { key     = function(z) return z end,
           most    = 256   },
  tree = { step    = 0.5   ,
           trivial = 1.05  },
  divs = { step    = 0.5    ,
           most    = 256    ,
           magic   = 2.56   ,
           epsilon = 0      ,
           skip    = "?"    ,
           fx      = function(z) return z end,
           depth   = math.maxinteger,
           cohen   = 0.3    ,
           trivial = 1.05  }
}
