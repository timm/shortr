# [:high_brightness: B(Ai)ttery](all.md)

<a href="all.md"><img align=right width=350 src="bat2.png"></a>

LUA is a "batteries-not-included" language.   But LUA makes it easy to add in the missing bits.
E.g. here are some "b(Ai)tteries" for XAI (explainable artifical intelligence).   


|what      | where |
|---------:|:------|
|start here| [object model](design.md) |
|config    | [all](all.md)   |
|build     | [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile) (just for doco)  | 
|demos     | [go](go.md)  |
|apps      | [nb](nb.md), [tree](tree.md)  |
|functions | [lib](lib.md) |  
|methods   | [bin](bin.md), [cols](cols.md), [num](num.md), [row](row.md), [rows](rows.md), [some](some.md), [sym](sym.md) |

<br clear=all>
<p align=center>
<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
</p>

## About our objects
One of the  theres here is that that there the thing we call "data 
mining" shares many of its internal data structures and algorithms
with the thing we call "optimization". So once we build those
internal things, then building "data miners" or "optimizers"
is a  pretty trivial extension. 

Just to demonstrate this, consider the object model of B(Ai)`ttery` :
-   `ROWS` holds many `ROW`s (and each `ROW` holds one record).  `ROWS` summarize their numeric
or symbolic  columns in `NUM`s or `SYM`s (respectively). Summaries are held in `COLS`, divided into  (x,y) sets for
independent and dependent columns (respectively). 
- `BIN`s and `SOME` are helper classes. Pairs of (x,y) columns are summarized in `BIN`s. Adjacent `BIN`s with  similar y distributions
are merged. 
`SOME` is a helper
for `NUM`s that holds just some sample of the numerics in that column. 
- Everything else is just tiny extensions to the above object model. e.g. 
  - When clustering, each cluster is its own `ROWS`.
  - `NB` classifiers create one `ROWS` per class in the training data.
  - Decision `TREE`s are built by recursively finding the `BIN`s that best distinguish different `ROW`s. 
  - etc.

## About XAI
From `Wikipedia` :

> Explainable AI (XAI), or Interpretable AI, or Explainable Machine
Learning (XML), is artificial intelligence (AI) in which the
results of the solution can be understood by humans. It contrasts
with the concept of the "black box" in machine learning where even
its designers cannot explain why an AI arrived at a specific
decision. By refining the mental models of users of AI-powered
systems and dismantling their misconceptions, XAI promises to help
users perform more effectively. XAI may be an implementation of
the social right to explanation. XAI is relevant even if there
is no legal right or regulatory requirement—for example, XAI can
improve the user experience of a product or service by helping end
users trust that the AI is making good decisions. This way the aim
of XAI is to explain what has been done, what is done right now,
what will be done next and unveil the information the actions are
based on. These characteristics make it possible (i) to confirm
existing knowledge (ii) to challenge existing knowledge and (iii)
to generate new assumptions.

> Recent scholarship has suggested that the pursuit of explainability
in AI techniques should be considered a secondary goal to the pursuit
of AI's effectiveness, -- and that encouraging the exclusive
development of XAI may limit the functionality of AI more broadly.

In reply to the last para, I offer my own experience where the trade-off was not "explanation or performance". Rather, by exploring concepts of XAI I an actually make the while
system better, faster, easier to understand. For example. here I offer
explaninable semi-supervised multi-objective optimization XAI
(from N items, find and explain the best ones, using just log(N) evals). This works as follows.
- PASS1 (guess): eval two distant items on multi-objective criteria.
       Prune everything nearest the worst one. Recurse on rest.
- PASS2 (guess again): do it again, using better items from first pass.
- PASS3 (explain): recursively discretize attributes on how well they
      distinguish the best and worst items (seen in second pass).

