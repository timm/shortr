package = "shortr"
version = "1.0-1"
source = {
   url = "git://github.com//timm/shortr",
   tag = "1.0-1"
}
description = {
   summary = "Semi-supervised multi-objective optimization XAI",
   detailed = [[
    From N items, find and explain the best ones, using just log(N) evals.

    PASS1 (guess): eval two distant items on multi-objective criteria.
          Prune everything nearest the worst one. Recurse on rest.  

    PASS2 (guess again): do it again, using better items from first pass.  

    PASS3 (explain): recursively discretize attributes on how well they
          distinguish the best and worst items (seen in second pass).]],
   homepage = "https://menzies.us/shortr/",
   license = "BSD/2clause" -- or whatever you like
}
dependencies = {
   "lua >= 5.1, <= 5.4.4"
}
build = {
   type = "builtin",
   copy_directories = { "data" },
   modules={
     shortr="shortr.lua",
     lib="lib.lua"}
}

