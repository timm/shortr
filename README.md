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
         
<a href="div.png"><img align=right width=225 src="div.png"></a>
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
        
**ASSIGNMENTS**

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
as genetic optimization [Ch18], but runs much faster.
- **Semi-supervised learning**: these applications require only the _2.log(N)_ labels at
of the pair of furthest points seen at each level of recursion.
- **Privacy**
- **Planning**
- **Monitoring**

