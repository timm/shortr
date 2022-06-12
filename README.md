
<img width=300 align=right src="https://raw.githubusercontent.com/timm/shortr/master/docs/img/cup.png">

# SHORTR.lua

[code](https://menzies.us/shortr)

<a
href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img 
src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a> <a 
href="https://zenodo.org/badge/latestdoi/206205826">
<img src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a>

Do I understand "it"?
Can I code "it" succinctly? Using simple tools and a minimal set of data 
structure and algorithms? Does "it" do the minimum work  (so"it" runs fast)?
 Can I explain "it" to you, quickly and successfully? And does "it" generalize
(so "it" is not a one-off hack)?

Let's check. Please review my code. This is  a multi-objective semi-supervised explanation tool. The code:

- Explores N points via  O(log2(N)) evaluations. 
- Generates a human-readable summary of that space. 
- In pass1, it find and eval  two distant points using multi-objective criteria. Everything nearest the  worst is pruned and we recurse on the rest.  
- This algorithm is only approximate so, in pass2, we do it all again, starting with the better items seen in pass1.  
- Finally, it explains the final results by  a decision tree that recursively discretizes numerics via their ability to distinguish the best/worst things found in pass2.


All in under 400 lines of code
 (most of which is background stuff that could be used for other learners).    


Share and enjoy!<hr>  
 

__Mother Teresa:__   
"The more you have, the more you are occupied. The less you have, the more free you are."

__Ken Thompson:__        
"One of my most productive days was throwing away 1,000 lines of code."

__William of Occam:__      
"It is vain to do with more what can be done with less."

__Donald Knuth:__     
"It is much more rewarding to do more with less."

__Leonardo da Vinci:__       
"Simplicity is the ultimate sophistication."

__Edsger Dijkstra:__        
"Simplicity is prerequisite for reliability."

__Dieter Ram:__       
"Less, but better."

__timm:__     
"plz, less"

<hr> 

Role models  (whose code challenges me to write less, but better):   
[Jack Diederich](https://www.youtube.com/watch?v=o9pEzgHorH0) 
| [Hilary Mason](https://boingboing.net/2017/06/30/next-level-regexp.html)
| [Brian Kernighan](https://www.oreilly.com/library/view/beautiful-code/9780596510046/ch01.html)
| [Peter Norvig](http://norvig.com/lispy.html)
| [Joel Grus](https://github.com/joelgrus/data-science-from-scratch)
| [Paul Graham](http://www.paulgraham.com/onlisp.html)
