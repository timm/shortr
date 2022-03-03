<img src="https://img.shields.io/badge/purpose-se,ai-informational?style=flat&logo=hyper&logoColor=white&color=red"> <img 
src="https://img.shields.io/badge/purpose-learner,optimizer-informational?style=flat&logo=hyper&logoColor=white&color=ff69b4"> <img 
src="https://img.shields.io/badge/language-lua-informational?style=flat&logo=lua&logoColor=white&color=yellow"> <img 
src="https://img.shields.io/badge/platform-osx,linux-informational?style=flat&logo=linux&logoColor=white&color=orange"> <a
href="https://github.com/timm/l5/actions/workflows/tests.yml"><img src="https://github.com/timm/l5/actions/workflows/tests.yml/badge.svg"></a> <a 
href="https://zenodo.org/badge/latestdoi/206205826"> <img src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a><br><img 
align=right width=175 src=docs/head.png><small>
<b><a href="https://menzies.us/l5">Docs</a> •  <a href="https://github.com/timm/l5/blob/master/LICENSE.md">&copy;2022</a></b>, Tim Menzies, <timm@ieee.org></small>


#  Do you understand AI?

How well do  you understand AI? 
Do you want to understand it better?  Want to practice that skill, a little more?

If you answering "yes", here's a little exercise for you.

- Step1: List out down what you think an AI toolkit should do.
Don't say "deep learning" or "clustering"-- those are just solution technologies.
Think instead of what services people want to get out of an AI. Things like
"watch for problems" or  "help me debate ideas with my friends" or "learn a summary
of what is going on right now" or "solutions we can trust".
- Step2:  Now,  code 
 up those services, as succinctly as can (say, in less than 1000 lines of code). Try to find
 patterns in the processing, such all the parts of your code are used
 in as many different ways as possible,
 And no cheating 
(so no importing of some massive background AI library).

If you try this, and show  your list to someone else, straight away you will learn that
"AI" is not one thing. Different people will list different things, depending on their
training and experiences. For the record, I'm all about using AI to help people understand
the world around them so my "AI" tries to find useful and succinct symbolic summaries. Other
people might prefer (e.g.)  sub-symbolic neural approaches. No worries, we can still be friends. And
maybe when those folks want succinct explanations of the systems they are exploring,
they might remember this code. 

But lets get to some specifics.
Buse and Zimmermann[^Bu12] surveyed over  hundreds of developers and managers to
find a list of "information needs" for software analytics. 
Lets make sure we understand these, enough, to code them succinctly.

![docs/bi.png](docs/bi.png)

(Aside: this list is not as complete as I'd like.
current concerns about FAT (fair, accountable, trust) or bias
mitigation.
Nor does it really touch on knowledge acquisition or
or how to trade
off goals between competing stakeholders.
But that's ok-- we can add that i and still not break
1000 lines of code).


[I](I) use this exercise  to teach software engineers what goes on inside AI.
My premise is that
the AI-literate engineering should be able to mix and match AI tools
  to create specific solutions for specific problems.  
To show then what goes on inside the box,
I ask them to:

- Reproduce this code in whatever language they like (except the one used here). That takes 6-8 weeks.
- Find all the short-cuts in this code, then find other AI tools that another approach to those short-cuts.
- Benchmark this tiny toolkit against those more elaborate tools. This is end-of-semester project, which takes
  another 6-8 weeks.

## Why is all these coded in LUA?

- Because Lua is fun to write and [easy to learn](https://learnxinyminutes.com/docs/lua/).
- Because I want you to learn AI by coding it up from scratch, but I do not want to 
  give you a fully worked solution.  So here's my code-- and your job is to recode it.
- Because LUA is great for teaching since 
  it installs, very quickly, on most platforms. This means that 
  this code can serve as an executable specification
  that students can use to check the output of their own code.
- Because LUA supports multiple programming methods, including procedural, 
  object-oriented, functional, and data-driven programming. You can also use it to
  write you own [domain-specific alnguages](https://www.lua.org/wshop11/luaws11_ag.pdf).
- Also, for hyperparameter optimziation, LUA has an interesting special advantage
  - A LUA name space is a very simple, very regular thing.
    If I load  a file twice (using `dofile`) then I get **two** copies of the namespace of that code. 
  - Which means I can write an optimizer in LUA
    and use that optimizer to optimize itself (in another namespace) 
    which almost no chance (\*)  of variables  in one space messing with the other

(\*) Of course, nothing is 100% safe. If one namespace reset the random number seed, 
that change can spread to the other space. Ditto with any other
global defined in the background LUA libraries.

<br clear=all><hr>
    
The need for baselines. XXXX

Standard supervised learners assume that all examples have labels.
When this is not true, then we need tools to incrementally 
([a](a)) summarize what has been seen so far; (b) find and focus
on the most interesting part of that summary, (c) collect
more data in that region, then (d) repeat.
         
<a href="docs/div.png"><img align=right width=225 src="docs/div.png"></a>
To make that search manageable, it is useful to exploit a 
manifold assumption; i.e.
higher-dimensional data can be approximated in a lower dimensional
manifold without loss of signal[^Ch05] [^Le05].
Manifolds lead to _continuity_
effects; i.e. if there are fewer dimensions, then there are more
similarities between examples.
Continuity simplifies _clustering_
([and](and) any subsequent reasoning).  More similarities means  easier
clustering. And after clustering, reasoning just means reason about
a handful of examples (maybe even just one)  from each cluster.
        
**HOMEWORKS**



- **Scripting**: little languages (e.g. regular expressions); test-drive-development, 
  pseudo-random numbers (and seeds), dialog independence, CLI design, version control, GitHub workflows, test-driven development, open science
- Data layer: summarize text files as samples of data
- Anomaly detection: Nearest-neighbor, Clustering
- Validation: effect size, significance texts
- Semi-supervised learning
- Regression, model trees.
- Discretization, explanation
- Privacy
- Planning, monitoring
- Optimization
- Bias, Fairness mitigation

- classification = Egs + likelihood
- anomaly detection = clustering + outliers
   - runtime monitoring = anomaly detection
- semi-supervised earning (ssl) = clustering + sampling
- privacy = semi-supervised + mutation
- optimization = ssl + domination
   - hyper parameter optimization = optimization
- Explanation = clustering + Discretization
   - visualization = explanation
   - contrast = explanation + pruning
   - planning =  contrast
   - monitoring = planning (with reversed weights)
   - bias mitigation = hyper parameter optimization
- Transfer elarning
   - Belief revision = incremental theory revision

How:

-  **Overlays** are predictions associated with each cluster; e.g. the
   mean and standard deviation of known class values in each
   cluster
-  **Goals** compare system performance with respect to some
   desired values. This is just the overlay values minus the
   goal values. If displayed over a diagram like Figure 2.d, he
   managers can quickly see how well (or how badly) different
   projects are performing with respect to current goals.
-  **Benchmarks** compare system performance with respect to
   established baselines. Like goals, this is just the overlay values
   minus the goal values.
-  If we tracked how project changes resulted in a project
   migrating around Figure 2.d then:
–  Past **trends** would be the track seen in historical data;
–  Forward **trends** would be an extrapolation of the past
   trend.
-  **Forecasts** would be just be predictions resulting from mapping
   a project into a cluster then predicting the properties of that
   project from the other examples in this cluster. Future forecasts
   could be implemented by applying the forecast method to the
   clusters seen in the forward trend.
-  **Simulations** could be implemented in two ways:
   1. For **simulation** via lookup, just generate something like
      Figure 2.d then read off the predictions for class values
      seen in each cluster. In this approach, the clustering
      process is like a what-if query that groups the data into
      sets of related possibilities.
   1. For **simulation** via execution, some domain model could
      be executed using inputs drawn from the clusters of
      Figure 2.d. In this approach, the clustering process divides
      the input space of the executable, after which we can
      sample different modes of the sample by sampling for
      different clusters.
-  **Alerts** could raised if new data does not fit into the old clusters.
   To implement such alerts, we use the dendogram that generated
   Figure 2.d:
   1. For each leaf cluster, randomly select pairs of instances
      (say, 100 times). Record the distribution of distances
      found in that sample.
   1. Take new data and walk it down the dendogram to find
      the leaf cluster. The new data is alien if it is an outlier
      on the distribution generated by step 1.

Note that if the results of step 1 are pre-computed and cached,
then step 2 could report anomalies in time _O(log(N))_ ; i.e. just
the time required to map new data down the dendogram to a
leaf cluster,

**ASSiGNMENTS**

- **Instance selection**: filter the data down to just a few samples per
cluster, the reason using just those.
- **Anomaly detection**:
- **Explanation** 
Discretize the numeric ranges (\*) at each level of the recursion,
then divide the data according what range best selects for one half, or the other
at the data at this level of recursion.
- **Multi-objective optimization:** This code
can apply Zitzler's multi-objective ranking predicate[^Zit04] to prune the worst
half of the data, then recurs on the rest[^Ch18]. Assuming a large over-generation
of the initial population (to say, 10,000, examples), this can be just as effective
as genetic optimization[^Ch18], but runs much faster.
- **Semi-supervised learning**: these applications require only the _2.log(N)_ labels at
of the pair of furthest points seen at each level of recursion.
- **Privacy**
- **Planning**
- **Monitoring**

## TOC

- Some theory
  - nothing as useful as a good theory
  - kelly. understand the world via . cluster and contrast
    - unifnying framework srteching for initilaiz conceptualization internal to a sysrtem
  - constrasts
    - difference between things shorter than the things
  - super discretization
    - consider everything we want for good and bad cluster. 
    - tabu, plan, monitor, optimization , explain
    - all ways to assess ranges
    - repeated observastion: a few ranges are most powerful
    - so when we say cluster and contrast, we mean find the ranges that most
      distinguish things
    - of course we combine these ranges into model... but we might not have to
      - could just print_ the super ranges and stop
  - chi merge: botton up discretization to combine silimar ranges
  - fastmap (an approximation to pca)


[^Ah91]: Aha, D.W., Kibler, D. & Albert, M.K. [Instance-based   learning algorithms](https://link.springer.com/content/pdf/10.1007%2FBF00153759.pdf). Mach Learn 6, 37–66 (1991).  https://doi.org/10.1007/BF00153759

[^Bo98]: Boley, D., 1998.  [Principal directions divisive partitioning](https://www-users.cse.umn.edu/~boley/publications/papers/PDDP.pdf) Data Mining and Knowledge Discovery, 2(4): 325-344.

[^Bu12]: Buse, R. and Zimmerman T. [Information Needs for Software Development Analytics](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/MSR-TR-2011-8.pdf), ICSE'12, 2012

[^Ch05]: [Semi-Supervised Learning](http://www.molgen.mpg.de/3659531/MITPress--SemiSupervised-Learning) (2005) Olivier Chapelle,  Bernhard Schölkopf, and Alexander Zien (eds).  MIT Press.
 
[^Ch18]: [Sampling” as a Baseline Optimizer for Search-Based Software Engineering](https://arxiv.org/pdf/1608.07617.pdf), Jianfeng Chen; Vivek Nair; Rahul Krishna; Tim Menzies IEEE Trans SE, (45)6, 2019

[^Ch22]: [Can We Achieve Fairness Using Semi-Supervised Learning?](https://arxiv.org/pdf/2111.02038.pdf) (2022), Joymallya Chakraborty, Huy Tu, Suvodeep Majumder, Tim Menzies. 

[^Fal95]: Christos Faloutsos and King-Ip Lin. 1995. FastMap: a fast algorithm for indexing, data-mining and visualization of traditional and multimedia datasets. SIGMOD Rec. 24, 2 (May 1995), 163–174. DOI:https://doi.org/10.1145/568271.223812

[^Le05]: Levina, E., Bickel, P.J.: [Maximum likelihood estimation of intrinsic dimension](https://www.stat.berkeley.edu/~bickel/mldim.pdf).  In: Advances in neural information processing systems, pp. 777–784 (2005)

[^Ke92]: Kerber, Randy [ChiMerge: Discretization of Numeric Attributes](https://www.aaai.org/Papers/AAAI/1992/AAAI92-019.pdf), AAAI'92

[^Pl04]: Platt, John.  [FastMap, MetricMap, and Landmark MDS are all Nystrom Algorithms](https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/nystrom2.pdf) AISTATS (2005).

[^Zit04]: [Indicator-based selection in multiobjective search](https://link.springer.com/chapter/10.1007/978-3-540-30217-9_84) Eckart Zitzler , Simon Künzli Proc. 8th International Conference on Parallel Problem Solving from Nature (PPSN VIII
