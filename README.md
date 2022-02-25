**[Docs](https://menzies.us/l5) • [Repo](https://github.com/timm/l5) • [Issues](https://github.com/timm/l5/issues) • [&copy;2022](http://github.com/timm/l5.LICENSE.md)** Tim Menzies

#  About L5
       
<img align=right width=250 src=docs/head.png>

If we choose our AI tools not on their complexity, but
on their understandably, what would they look like?
To that end, I've been looking back over
common themes seen in my
AI graduate students (30+ students, over 20 years). What I was
after were the least lines of code that offer the most
AI functionality-- and which could be mixed and matched in
novel and interesting ways.
      
The result is this file. My standard "intro to AI" exercise is six
weeks of homeworks where students rebuild the following code,from
scratch, in any language they like (except LUA).  After that, 
students can review all the assumptions of this code, then read the
literature looking for other tools that challenge those assumptions.
That leads to a second a 4-6 week project using these tools as a baseline against
which they can compare other, more complex, approaches.
  
<br clear=all><hr>
    
The need for baselines. XXXX

Standard supervised learners assume that all examples have labels.
When this is not true, then we need tools to incrementally 
(a) summarize what has been seen so far; (b) find and focus
on the most interesting part of that summary, (c) collect
more data in that region, then (d) repeat.
         
<a href="docs/div.png"><img align=right width=225 src="div.png"></a>
To make that search manageable, it is useful to exploit a 
manifold assumption; i.e.
higher-dimensional data can be approximated in a lower dimensional
manifold without loss of signal [Ch05,Le05].
Manifolds lead to _continuity_
effects; i.e. if there are fewer dimensions, then there are more
similarities between examples.
Continuity simplifies _clustering_
([and](and) any subsequent reasoning).  More similarities means  easier
clustering. And after clustering, reasoning just means reason about
a handful of examples (maybe even just one)  from each cluster.
        
**HOMEWORKS**

- **Scripting**: little languages (e.g. regular expressions); test-drive-development, CLI design, version control, GitHub workflows, test-driven development, open science
- Data layer: summarize text files as samples of data
- Nearest-neighbor, Clustering
- Validation: effect size, significance texts
- Regression, model trees.
- Discretization, explanation
- Privacy
- Planning, monitoring
- Optimization
- Bias, Fairness mitigation

**ASSiGNMENTS**

- **Instance selection**: filter the data down to just a few samples per
cluster, the reason using just those.
- **Anomaly detection**
- **Explanation** 
Discretize the numeric ranges (\*) at each level of the recursion,
then divide the data according what range best selects for one half, or the other
at the data at this level of recursion.
- **Multi-objective optimization:** This code
can apply Zitzler's multi-objective rankining predicate [Zit04] to prune the worst
half of the data, then recurs on the rest [Ch18]. Assuming a large over-generation
of the initial population (to say, 10,000, examples), this can be just as effective
as genetic optimization[^Ch18], but runs much faster.
- **Semi-supervised learning**: these applications require only the _2.log(N)_ labels at
of the pair of furthest points seen at each level of recursion.
- **Privacy**
- **Planning**
- **Monitoring**

## References

[^Ah91]:
Aha, D.W., Kibler, D. & Albert, M.K. Instance-based   
learning algorithms. Mach Learn 6, 37–66 (1991). 
https://doi.org/10.1007/BF00153759

[^Bo98]:
 Boley, D., 1998. 
[Principal directions divisive partitioning](https://www-users.cse.umn.edu/~boley/publications/papers/PDDP.pdf)
 Data Mining and Knowledge Discovery, 2(4): 325-344.

[^Ch05]:
[Semi-Supervised Learning](http://www.molgen.mpg.de/3659531/MITPress--SemiSupervised-Learning)
(2005) Olivier Chapelle,  Bernhard Schölkopf, and Alexander Zien (eds). 
MIT Press.
 
[^Ch18]: 
[Sampling” as a Baseline Optimizer for Search-Based Software Engineering](https://arxiv.org/pdf/1608.07617.pdf),
Jianfeng Chen; Vivek Nair; Rahul Krishna; Tim Menzies
IEEE Trans SE, (45)6, 2019

[^Ch22]: [Can We Achieve Fairness Using Semi-Supervised Learning?](https://arxiv.org/pdf/2111.02038.pdf) (2022), Joymallya Chakraborty, Huy Tu, Suvodeep Majumder, Tim Menzies. 

[^Fal95]: 
Christos Faloutsos and King-Ip Lin. 1995. FastMap: a fast algorithm for indexing, data-mining and visualization of traditional and multimedia datasets. SIGMOD Rec. 24, 2 (May 1995), 163–174. DOI:https://doi.org/10.1145/568271.223812

[Le05]:
Levina, E., Bickel, P.J.: [Maximum likelihood estimation of intrinsic dimension](https://www.stat.berkeley.edu/~bickel/mldim.pdf). 
In:
Advances in neural information processing systems, pp. 777–784 (2005)

[^Ke92]:
Kerber, Randy [ChiMerge: Discretization of Numeric Attributes](https://www.aaai.org/Papers/AAAI/1992/AAAI92-019.pdf), AAAI'92

[^Pl04]: 
Platt, John. 
[FastMap, MetricMap, and Landmark MDS are all Nystrom Algorithms](https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/nystrom2.pdf)
AISTATS (2005).

[Zit04]:
[Indicator-based selection in multiobjective search](https://link.springer.com/chapter/10.1007/978-3-540-30217-9_84)
Eckart Zitzler , Simon Künzli
Proc. 8th International Conference on Parallel Problem Solving from Nature (PPSN VIII
