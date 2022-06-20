<img align=right width=150 src="https://raw.githubusercontent.com/timm/shortr/master/docs/img/cup.png">
    
# [Shortr.lua](README.md)
Semi-supervised multi-objective optimization XAI.   
From N items, find and explain the best ones, using just log(N) evals.
     
[data](data.md) :: 
[num](num.md) :: 
[range](range.md) :: 
[row](row.md) ::
[sym](sym.md) :: 
[lib](lib.md)  
         
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>
<a href="https://opensource.org/licenses/BSD-2-Clause"><img  src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg"></a>
<a href="https://zenodo.org/badge/latestdoi/206205826"> <img src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a>

> __Dieter Rams:__      
_"Less, but better."_

> __Alan Perlis:__    
_"Simplicity does not precede complexity, but follows it."_ 

> __Tony Hoare:__    
_"Inside every large program is a small program struggling to get out."_

Now that you've done _it_, did you really understand _it_? Let's check.
Can you do _it_ better?
Can you now
write _it_ in fewer lines and do you know how to make _it_ run faster?
Can you see how _it_ is same/different to other things?
And can you use those similarities to do more things with  _it_? 
Finally, can you teach _it_ quickly to newcomers?

E.g. do I understand a multi-objective semi-supervised XAI?
Well, maybe.  Here I search
N items, find and explain the best ones, using just log(N) evals:

- PASS1 (guess): eval two distant items on multi-objective criteria.
        Prune everything nearest the worst one. Recurse on rest.  
- PASS2 (guess again): do it again, using better items from first pass.  
- PASS3 (explain): recursively discretize attributes on how well they
         distinguish the best and worst items (seen in second pass).

Note that once 
I build that, decision trees, Naive Bayes classifiers,
and nearest neighbors were all tiny extensions. 
Also included here
is literate programming, self-documenting code and support for
test-driven development. 
All in a few hundreds  lines of LUA: <br>
`awk '!/^(--|[ \t]*$)/{n++} END {print n" lines"}' *.lua`   => 511 lines
     
Share and enjoy.
