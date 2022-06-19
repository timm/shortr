

   <img width=200 align=right src="https://raw.githubusercontent.com/timm/shortr/master/docs/img/cup.png">
 

# shortr.lua:<br>  less, but better XAI. :eyes:


 Semi-supervised multi-objective optimization XAI.   
From N items, find and explain the best ones, using just log(N) evals.
 

 <a href="http://menzies.us/shortr"> <img align=left src="docs/img/docs.png" width=200></a><br>  
  <a  
href="https://zenodo.org/badge/latestdoi/206205826"> <img src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a><br>
 <a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a>  
 <p>&nbsp;
 
 
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
Well, maybe. Here's all that, most of which is
background stuff that could be used for other learners.
Once
I build that, I found that decision trees, Naive Bayes classifiers,
and nearest neighbors were all tiny extensions. 
Also included here
is literate programming, self-documenting code and support for
test-driven development. 

All in around 300 lines of LUA: <br>

`awk '!/^(--|[ \t]*$)/{n++} END {print n" lines"}' *.lua`   => 301 lines
     
Share and enjoy.



### Coding conventions 
Before reading this code, it might be best to   [review  these local coding conventions](https://github.com/timm/shortr/blob/master/CONTRIBUTE.md).

And why, you may ask, is this written in LUA? Well. the whole point of this code is make people say "hey, that
   is so easy I can write it in Rust, Typescript, Python, Julia, Erlang,  Ruby, etc, etc...". So this code steers
   clear of clever coding practices
   that might not easily port. Off-the-shelf LUA has less than 2 dozen built-ins-- which makes it pretty useful as a simple executable specification system.

   (Aside: also, for me, LUA is kinda fun to use.)
   
 

### Role Models
Their work dares me to write less, but better: [Jack Diederich](https://www.youtube.com/watch?v=o9pEzgHorH0) 
, [Hilary Mason](https://www.youtube.com/watch?v=l2btv0yUPNQ)
, [Brian Kernighan](https://www.oreilly.com/library/view/beautiful-code/9780596510046/ch01.html)
, [Joel Grus](https://github.com/joelgrus/data-science-from-scratch).
, [Brian McFee](https://brianmcfee.net/papers/ismir2011_sptree.pdf).    
As do my fav LISPers: 
([Peter Seibel](https://gigamonkeys.com/book/)
  ([Conrad Barski](https://doc.lagout.org/programmation/Lisp/Land%20of%20Lisp_%20Learn%20to%20Program%20in%20Lisp%2C%20One%20Game%20at%20a%20Time%20%5BBarski%202010-11-15%5D.pdf)
  ([Paul Graham](http://www.paulgraham.com/onlisp.html)
    ([Peter Norvig](http://norvig.com/lispy.html)
      ([Guy Steele](https://dspace.mit.edu/bitstream/handle/1721.1/5790/AIM-353.pdf?sequence=2&isAllowed=y)))))).
 



