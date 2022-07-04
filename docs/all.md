# [:high_brightness: B(Ai)ttery](all.md)

<a href="all.md"><img align=left width=400 src="bat2.png"></a>

LUA is a "batteries-not-included" language.   
But LUA makes it easy to add in the  missing bits.   
E.g. here are some "b(Ai)tteries" for XAI (explainable artifical intelligence).   

(c) 2022, Tim Menzies <timm@ieee.org>

|what          | where |
|-------------:|:------|
|**config**    | [all](all.md)   |
|**build**     | [Makefile](https://github.com/timm/shortr/blob/master/etc/src/Makefile) (just for doco)  | 
|**demos**     | [go](go.md)  |
|**apps**      | [nb](nb.md), [tree](tree.md)  |
|**functions** | [lib](lib.md) |  
|**methods**   | [bin](bin.md), [cols](cols.md), [num](num.md), [row](row.md), [rows](rows.md), [some](some.md), [sym](sym.md) |

In this code :
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

<p align=center>
<a href=".."><img src="https://img.shields.io/badge/Lua-%232C2D72.svg?logo=lua&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/Linux-FCC624?logo=linux&logoColor=black"></a>
<a href=".."><img src="https://img.shields.io/badge/mac%20os-000000?logo=apple&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/VIM-%2311AB00.svg?logo=vim&logoColor=white"></a>
<a href=".."><img src="https://img.shields.io/badge/checked--by-syntastic-yellow?logo=Checkmarx&logoColor=white"></a>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg?logo=opensourceinitiative&logoColor=white"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img  src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a> 
</p>


## About XAI and b(Ai)ttery
Explainable AI (XAI) is a subset of AI that tries
to build models that people can read and understand and critique and easily change. B(Ai)ttery
is a small set of classes that implements a few interesting
XAI tools. 

For the "big picture" on XAI, see the Vilone and Logno' 2020 systematic review.



```lua
--
```


For a small set of really useful XAI tactics, see below.



```lua
--
```


### About XAI
For years
I used   XAI to _augment_ other more opaque AI tools.
But that meant I was not explaining the real inference
process, just some frail copy of what was really going on.

Then I found that XAI can (sometimes) actually 
_replace_ other AI tools since, at least in the domains
I've explored, XAI tools can make better conclusions, 
faster, and those conclusions are explicable to people.



```lua
--
```


(Not always of course. If you gave me 10,000 wavelets
from a signal processing package then of course I'd 
reach for a deep learner.
But if you wanted to _tune_ that deep
learner, then I'd still use this code since it
just runs a few what-of queries on the
most important parts of the data.)

XAI should be designed with an understanding of human
cognitive processes. People are clever, as Davenport and Beck remind us,
they  have [fixed and limited attention spans
which they  hoard and use sparingly.
Herbert Simon say that humans  use heuristic "short cuts" that let 
them satisfy the demands of their work, just enough
before rushing off to their next
task.

Once such short-cut is the "cue"; i.e. a small range
of some variable that most effects the outcome. Feature
extraction and weighting  is the process of finding
those cues. This code can be summarized as "the hunt
for 'cues'". 

Another short-cut is sampling; i.e. don't look at 
everything, just a few things. There are many ways to
sample and this code exploits them all (random, 
reservoir, extreme).



```lua
--
```


### References
- Thomas H. Davenport and John C. Beck. (2001). 
  [The Attention economy](`https` ://ubiquity.acm.org/article.cfm?id=376626). 
  Ubiquity 2001, May (May 1 - May 31, 2001), 
- Gigerenzer, G. (2008). 
  [Why Heuristics Work](`https` ://pure.mpg.de/rest/items/item_2100099/component/file_2100098/content).
  Perspectives on Psychological Science, 3(1), 20–29. 
- Vilone, Giulia & Longo, Luca. (2020). 
  [Explainable Artificial `Intelligence` : a Systematic Review](`https` ://arxiv.org/pdf/2006.00093.pdf)
- Simon, Herbert A. (1956). 
  [Rational Choice and the Structure of the Environment](`https` ://uk.sagepub.com/sites/default/files/upm-binaries/25239_Chater~Vol_1~Ch_03.pdf)
  Psychological Review. 63 (2): 129–138.



```lua
local all=require"lib"
all.the = all.opts( [[
BAITTERY: semi-supervised multi-objective optimization XAI
(c) 2022 Tim Menzies <timm@ieee.org> BSD2 license
     
From N items, find and explain the best ones, using just log(N) evals.
PASS1 (guess): eval two distant items on multi-objective criteria.
      Prune everything nearest the worst one. Recurse on rest.  
PASS2 (guess again): do it again, using better items from first pass.  
PASS3 (explain): recursively discretize attributes on how well they
      distinguish the best and worst items (seen in second pass).
   
USAGE:
  lua go.lua [OPTIONS]
   
OPTIONS:
  -M  --Min    min size of space                    =  .5
  -b  --bins   max number of bins                   =  16
  -F  --Far    how far to look for remove points    =  .95
  -k  --k      Bayes hack: low attribute frequency  =  2
  -m  --m      Bayes hack: low class frequency      =  1
  -p  --p      distance coefficient (2=Euclidean)   =  2
  -s  --seed   random number seed                   =  10019
  -S  --Some   max number of nums to keep           =  256
  -w  --wait   wait this number before testing      =  10
   
OPTIONS (other):
  -f  --file   file           =  ../../data/auto93.csv
  -g  --go     start-up goal  =  nothing
  -h  --help   show help      =  false ]])

return all
```


XX
This code contains 
B(Ai)TTERY (a set of AI-related classes) and 
 various AI tools, coded on top of B(Ai)TTERY. 

One of the  idea here is that that there the thing we call "data 
mining" shares many of its internal data structures and algorithms
with the thing we call "optimization". So once we build those
internal things, then building "data miners" or "optimizers"
is a  pretty trivial extension. 



```lua
--
```


### Apps
 Naive Bays Classifier

Trees (regression and decision)
 
 Recursive random projections
 
`SHORTR` :
Semi-supervised multi-objective optimization XAI
(from N items, find and explain the best ones, using just log(N) evals).
PASS1 (guess): eval two distant items on multi-objective criteria.
       Prune everything nearest the worst one. Recurse on rest.
PASS2 (guess again): do it again, using better items from first pass.
PASS3 (explain): recursively discretize attributes on how well they
      distinguish the best and worst items (seen in second pass).

### Coding conventions 
Before reading this, it might  be best to    
review these [local coding conventions](`https` ://github.com/timm/shortr/blob/master/CONTRIBUTE.md).
## Why this code?
 This code is an experiment in "less-is-more". Death to mash-ups and their associated 
 problems with technical debt and security problems that leak in from all 
 the parts used in the assembly.



```lua
--
```


<b>Tony `Hoare` :</b><br>
<em>"Inside every large program is a small program struggling to get out."</em><p>
<b>Alan `Perlis` :</b><br><em>"Simplicity does not precede complexity, but follows it."</em><p>
<b>Dieter `Rams` :</b><br><em>"Less, but better."</em>



```lua
--
```


Now that you've done _it_, did you really understand _it_? Let's check.



```lua
--
```


Can you do _it_ better?
Can you now
write _it_ in fewer lines and do you know how to make _it_ run faster?
Can you see how _it_ is same/different to other things?
And can you use those similarities to do more things with  _it_?
Finally, can you teach _it_ quickly to newcomers?



```lua
--
```


E.g. do I understand a multi-objective semi-supervised explanation algorithms?
Well, Let's check. 



```lua
--
```


Here's all that, most of which is coded in B(Ai)TTERY
that could be used for other learners.  

Also included here is literate programming,
self-documenting code and support for test-driven development.
All in around 500 lines of `LUA` : <br>



```lua
--
```


`awk '!/^(--|[ \t]*$)/{n++}`     
`END {print n" lines"}' *.lua`  
=> 500 lines
     
Share and enjoy.



```lua
--
--
```


### Role Models
People that inspire me to code less, but `better` :<br>
[Jack Diederich](`https` ://www.youtube.com/watch?v=o9pEzgHorH0), [Hilary Mason](`https` ://www.youtube.com/watch?v=l2btv0yUPNQ),
[Brian McFee](`https` ://brianmcfee.net/papers/ismir2011_sptree.pdf),  
[Brian Kernighan](`https` ://www.oreilly.com/library/view/beautiful-code/9780596510046/ch01.html),
[Joel Grus](`https` ://github.com/joelgrus/data-science-from-scratch).<p>
Especially the `LISPers` : <br>
([Peter Seibel](`https` ://gigamonkeys.com/book/)
  ([Conrad Barski](`https` ://doc.lagout.org/programmation/Lisp/Land%20of%20Lisp_%20Learn%20to%20Program%20in%20Lisp%2C%20One%20Game%20at%20a%20Time%20%5BBarski%202010-11-15%5D.pdf)
  ([Paul Graham](`http` ://www.paulgraham.com/onlisp.html)<br>
    ([Peter Norvig](`http` ://norvig.com/lispy.html)
      ([Guy Steele](`https` ://dspace.mit.edu/bitstream/handle/1721.1/5790/AIM-353.pdf?sequence=2&isAllowed=y)))))).
