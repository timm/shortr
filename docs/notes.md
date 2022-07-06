n-1 globals is better than n

old debate : buy vs build? traditional answer: buy the most rebuild the elast. not 
everypne that way

keep control of the  seed


Understanding AI (a friedly approach)

software deisng is about choices. sysmtes that seem fixed, unchangabel, enternal
are actually comprised on millions of choices that someone has made. If we don't
like something and we can't see how to change it, that might means we
don't know the choice points and how to influence them.

take AI for example. do we like that kind of  black box AI systems
taht use opagqq agorihsm running over models that only a small number of large corporaitas
can generate ? which offer no understanding of their internal struture? with a 
long track record of making biased or polarizing decisions that adversial effects
not only subgroups in our society, but aos threatens the very idea of policy making
via considered rational debate?

or do we want another kkidn of AI, one that we can understand and that understands
our limitations? And this other kind of AI is more of a friend that helps us:
- make and  monitor new decisions
- find and fix problems with old decisions 

sure sometimes the data is so complex that we need that first kind of AI that discovers
and exploits patterns far beyond the power of mere mortals. but all problems are not
like that. and even if they were, for complex decisions that  mission or
safety critical, then we must at least augment (and perhaps, even, repalce)
that kind of AI with another kinds of friendlier, understandable. AI.

having thought about this for a while, i'm wondering if there is something that's been
overlooked. some "thing" we can learn from decades of working with these tools that might
simplify the thing we call AI and make it friendlier and more understandable. this book
is about two such things:

THING1: Some "thing" learned over decades of AI is that the reason most of the systems
work is that complex data can often be approximated by a much simpler data set
taht can be described using far fewer variables. This is called the "manifold" effect 
and is the basis of much cirrent work in (e.g.) semi-supervised learning. 
Another "thing" that is good to know about manifolds is that within, there is often just
a few settings to variables, or just a few key examples, that let us distinguish one thing
from another.

THING2: maybe we don't understand AI cause we don't understand understanding. decision
making is not some hard nad fces certain process. its mre a best guess pulled from a space
of possibities. but most people don;t seem to appreciate that. If you doubt that, then
show people two overlapping bell-shaped curves and walk them, left to right, over the x-axis. Where
do they draw the line that seperates one group from the other? And what happens if we don't
have one axis, but dozens (each with different overlap)? and what if the shapes we are talking
obout aren't pretty little bell curves but real world data that can any darned shape at all?

Lets try another kind of AI that takes advantage of THING1 and THING2. THING1 tells us
that a good tactic when faced with a new problem is to drill down to find the keys
(the underlying attribtesa andsettings that most effect the system)..
And THING2 tells us taht if we want understandable AI, we better start out looking at how
humans understand things, then use that to build new kinds of AI algorithms.

This book is organized about THING1 and THING2. All the work here assumes that we don't
model all the data, rather, we just drive as fast as we can towards the keys attributes and settings.
Technically speaking, (and fell free not to skip over this bit),
we'll use a heterogenous distance metric to recusrively divde data via
random projections down to (say) clusters of size sqrt(N). Then we';l use multi-objective 
evalaution criteria and contrast set learning to select best clusters. Finally, a decision tree
learner will be applied to explain the difference between best and worst clusters.
algorithm

As for THING2, we'll start by looking at how humans understand and sample the  world around them.
Then we'll build tools to improve that understanding. Specifically, we'll start with seven sampling
policies seen in qualitative reaoning, then augment sampling such that it can support
nine common business intellgerence operatiosn (compare, contast, model, etc).
The sampling methods (used in software engienering) were documented in a recent paper by Ralph et.al. 
and the business intelligence operators come from some work at Microsoft by Buse and Zimmermann where 210 managers were asked
"what do they need from AI?". Along the way, tools from THING1 will be used as sub-routines to help us sample better.

There are several reasons to take this friendlier sampling approach to AI.
Firstly, it is human understandable (if you don't believe that, then please skim the rest of this thin book).

Also, right of the gate, we can discuss about  verification  and bias mitigation. Sampling means looking "here", but not necessarily
"there". How can we improve our sampling to minimize
the odds of missing important stuff? How can we sample to better access important
voices and data and improve what the system does?  Please contrast this with what happens in a traditional
AI subject that spends so much time on algorithms that ethics and verification become hasty footnotes at the end
of the subject (if mentioned at all).

Further, sampling comes with  MATHS! Beautiful simple maths based
on sampling theory that offers guarantees on  how many samples are needed such that we are within &epsilon;
of the best answer. Which means, right from the start, we can reason about what is going on within these systems (unlike other approach to AI).

Lastly, we can show many examples where this friendlier approach to AI  actually out-performs
prior state-of-the-art reasoning. It turns out that reasoning about people is a better way to reason about AI.
The cognitive limitations of humans means they ahve to be smarter about how they sample the world. And if we
implement those smarter sampling policies as algorithms, then we can do better than traditional AI.

(Do we do better all the time? 
No, of course not. There are so many examples of approaches that work well in one context and fail in another.
There's even a theory about that-- see the wonderfully named Wolpert's "no free lunch theorem"s. But our
results do tell us that it is not insane to try friendlier AI. Opague and complex deep learning algorithms
are not necessarily the best firt choice for a new domain. And even if you use those algorithms, you could
still find friendly AI useful as (a) a way to quickly partition a hard problem into several smaller and simpler parts;
(b) a way to tune your deep learner; (c) as an explaination tool to let people understand what you have done.)

------------------

tells us that for optimizera dn learnes
do
[age](age) of massh ups. long supply chains of code X depending on code y on code z.
changed sway we think about coding. MIT changed its first year subject to stop
looking deeping and instead went to mash up. we end up wu with nsrat's supply
chain exposures. or long build times. or siatuations where one peron pulling a 3 line package
can (at least, temporarily) break [the](the) internet.

been templted. in enironemtns that remward and promote impressive technical
accomplishmnet, the idea that pirrmive looking apps can be qucly wired together,
is certainly appelaing. and many start seeing that wiring together as itsself
a productiviity advantage and start processing scirting or meta-level
tools to sped up that wiing system (e.g. punlish compoentns as types that
now the types of their oo, offer a visual programming environemtn where anything
can be wited nto anything else via a GUI, provided the types being produced
by the upstream thing match the types required as input by the downstream thing.

but instead of wiring together large assemblies,s perhaps we need to be looking
for synthesis and simplification across our code base. another way to use a prior
code base is to treat it like the results of an experiment. that work found out
of ABCDEF.. that AB and DE was most useful. also, looking over AB and DE we can see
a way toregactor them both sich that we can do both, in  less code. "THe best answer
is the one that destroys the question" said Susan Sontag and perhaps she was writing.
Perhaps old code shuld be viewed as a corpse that we, as code scavnegers, pick over
looking for the tastiest morsels.

Just to be clear, constantly reviewing and refactoring old code into new, simpler,
systems is not recommended in hfast pasced environments when all that matters
is getting a product out the door in order to generate the cash flow that lets us
live another day. 
But there is some evidence that that approach has gone too far. model stores. awful
code, looking pretty.

So perhaps, sometimes, it is useful to sit back and calm down
and reflect to see what we've learned from the past to simplify the future.

For the reader who is feeling  calm and reflective today, we offer the following.

XX all the above could ahve been said many times since the dan of programm,ing. we have one new thing to
add:a repeated result about minimal models. momimal or not at all. manifold.

also, olicy fails. need simler ssystems. we doing things wrong. models tores..
____
examppes from my role model file
-----

but the best way. no best way. albert. but at least a quick way o(log(N)) initial ass uitable to define. refuce a larger slace to smaelr space of options.
or run as a postoorcessor to simplify some other more complex inference. or as inspiration for research "X works in shortr... soulf that work better if..." and then we cangt\and better shortr 2.0

also the code is offered, politely, as a suggestion that maybe it is not a good idea to hac together lots of existing code into a massive conglomieration.
suopply chain threats. comphrension. simplicity nderneatn


surprise: doco and tests take a large percent of the work

small support (see make file)

beware log(0) and hi==lo


surprises
- Col is a primary thing
- Data is a thing. cloning. reporting. clistering = make recursive tables

iit comes with maths (sampling). it comes with knowledge of humans (cognitive theory)
it comes wih v&v out ofthe box. it comes with explanaintion and oversight.

sampling: the larger the sample, the less we keep (reservoir)

1% rule

losts of little functions

type hints

literature programming
- self dcumenting code (make file help, code comment#1

+iterative diconphmzation: discretization, sctott-knot, decision tree
+
 self-documentating code:
- type hints
- help string
- make menus

 +
 +----
 +
 +creational patterns
 +singleton(s): the of pattern
 +prototype: clone
 +factory: dsl . headers
 +
 +
 +
 +----
 +
 +Structural patterns
 +These patterns explain how to assemble objects and classes into larger structures while keeping these structures flexible and efficient.
 +composition: ROWS: cols: num
 +             ROWS: ROW
 +
 +farcade: dsl for th headers
 +decorator: range is a decorator for SYM
 +----
 +
 +behavior:
 +These patterns are concerned with algorithms and the assignment of responsibilities between objects.
 +iterator (csv)  stragey command (goal)
 +visitor:: mid, div,bins



self-documentating code:

- Makefile
- sting text

have to get the 5 laws in here somehow

the important thing here is not the specific ai algorithms but the infrastructure around it. if we do word counts on this code base and count
how many line are specific to (e.g.)  knn or naive bayes etc , the interesting pattern is that most of the code
is NOT specific. that is this code is a worknecnhu of N lines of code within which it is fast to assemble a NB (10 lines) or a KNN (12 lines)
.

better yet, with when software contains many small utilities that share the same (much larger) underling infrastructure,
then it becomes easy to mix and match more; i.e. to combine parts of this and parts of that according to your whims
or the needs of the next problem.



Less is more

Made famous by the designer and architect Ludwig Mies van der Rohe,
the dictum, Less is More came to define the brave, utopian ideals
of modernist design and architecture. In fact the phrase originated
in Robert Browning's 1855 poem Andrea del Sarto.

https://www.poetryfoundation.org/poems/43745/andrea-del-sarto

Although the postmodernism movement of the Eighties and Nineties
aimed at redefining this notion (one of postmodernism's chief
chroniclers, architect Robert Venturi proclaimed 'Less is a bore' [https://www.archdaily.com/964625/what-is-postmodernism] ,
the original phrase is still used widely today, especially to chasten
artists and designers who may have got carried away with ideas of
their own brilliance.

----
static code analysis
formatting conventions
---
data independence

isepeateion mechaism and policy

encapsulation (infrmation hiding ; e.g. dont exprt help string)

technical debt. the set in the ego code. hard not too

less is more
- stop writing classes

- self docuenting code. documentaion is part of cde

rapid feedback
- making making
  https://m.facebook.com/nt/screen/?params=%7B%22note_id%22%3A347492333250958%7D&path=%2Fnotes%2Fnote%2F&_rdr
  https://www.youtube.com/watch?v=nIonZ6-4nuU
- tdd
  - refactoring, rule of three.
- tests different to code since most tests are
  platform specific and dont want to change the code code,
  just the platfrom specific bits
- GH workspaces; unix error conventions

version control

- OSS licenses
- code of conduct
- .gitignore
- standard directors etc docs src test

dialog independence

policy and mechanism 
  - regular expressions
  - doc string
  - data headers

factories (cols)

namespace

functionap programming
- iterators

poymorphism: 
  - delegastion
  - protocls e.g. mid, like,


linux kernel best pracrices
- short release cycles
  - how to make a really short feedback loop
  - link to TDD
- zero internal boundaries
  - everyone uses same tools, config files for those tools in repo
  - e.g. static code tools

--
y=f(x) what does different y,x mean

variance, entropy

ranges. erging

streaming

incremental

partial sample
  reserovour sampling

bottom up clustering


background ruler (geometry)
- stops errors in lower levels of clister

rows created once, shared in different tables.
- row becomes place to store staitics.

tables
  - summaries


stats
- effect size
- nootstreps 



