<span id="forkongithub"><a href="https://github.com/timm/shortr">Fork me on GitHub</a></span>

<em>"If you cannot - in the long run - (explain to)
    everyone what you have been doing,
    your doing  has been worthless."</em><br>- Erwin Schrodinger

<em>And I say, hey-ey-ey-a-ey, hey-ey-ey-ey
    I said "Hey, a-what's going on?"</em> <br>- 4 Non Blondes

AI and XAI (explainable artificial intelligence) need not be
hard.  E.g. here's a few hundred lines of LUA
to search N items to  find and explain the best ones, using just
log(N) evals.  

This code makes extensive use of a ROWS object.  Data from disk
becomes a ROWS. ROWS are recursive bi-clustered by partitioning on
the distance to two distant points (found after a few dozen random
projections).  Each cluster is new ROWS object, containing a subset
of the data. A decision tree is built that reports the difference
between the "best" and "worst" clusters (defined using a multi-objective
domination predicate) and that tree is just a  ser
of ROWS with `kids` pointer to sub-RWS).  This process
only needs log2(N) queries to y-values (while clustering,
just on the pairs of
distance objects).

This code starts with a help string (from which we extract our global settings)
and ends with a library of demos (see the `go` functions at end of file).  
Each setting can be (optionally) updated by a command-line flag.
Demos can be run separately or  all at once (using `-g all`).
  For regression tests, we report the failures seen when the demos run.

<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<[a](a) href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a><br>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
<br clear=all>
   
