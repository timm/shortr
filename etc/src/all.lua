-- ## About
       
local all=require"lib"
all.the = all.opts( [[
  
B(AI)TTERY:  (c) 2022 Tim Menzies, BSD2
LUA is a “batteries-not-included” language. So here, we add
some "batteries" to power some AI apps (focusing on XAI)
  
OPTIONS:
  -s  --seed    random number seed = 10019
  -S  --Some    max nums to keep   = 256
  
OPTIONS (other):
  -f  --file    data file        = ../../data/auto93.csv
  -g  --go      start up action  =  nothing,
  -h  --help    show help        =  false
]])
return all

-- This code contains 
-- B(Ai)TTERY (a set of AI-related classes) and 
--  various AI tools, coded on top of B(Ai)TTERY. 
-- 
-- One of the  idea here is that that there the thing we call "data 
-- mining" shares many of its internal data structures and algorithms
-- with the thing we call "optimization". So once we build those
-- internal things, then building "data miners" or "optimizers"
-- is a  pretty trivial extension. 
--
-- ### Apps
--  Naive Bays Classifier
-- 
-- Trees (regression and decision)
--  
--  Recursive random projections
--  
-- SHORTR:
-- Semi-supervised multi-objective optimization XAI
-- (from N items, find and explain the best ones, using just log(N) evals).
-- PASS1 (guess): eval two distant items on multi-objective criteria.
--        Prune everything nearest the worst one. Recurse on rest.
-- PASS2 (guess again): do it again, using better items from first pass.
-- PASS3 (explain): recursively discretize attributes on how well they
--       distinguish the best and worst items (seen in second pass).
-- 
-- ### Coding conventions 
-- Before reading this, it might  be best to    
-- review these [local coding conventions](https://github.com/timm/shortr/blob/master/CONTRIBUTE.md).
-- ## Why this code?
--  This code is an experiment in "less-is-more". Death to mash-ups and their associated 
--  problems with technical debt and security problems that leak in from all 
--  the parts used in the assembly.
--
-- <b>Tony Hoare:</b><br>
-- <em>"Inside every large program is a small program struggling to get out."</em><p>
-- <b>Alan Perlis:</b><br><em>"Simplicity does not precede complexity, but follows it."</em><p>
-- <b>Dieter Rams:</b><br><em>"Less, but better."</em>
--
-- Now that you've done _it_, did you really understand _it_? Let's check.
--
-- Can you do _it_ better?
-- Can you now
-- write _it_ in fewer lines and do you know how to make _it_ run faster?
-- Can you see how _it_ is same/different to other things?
-- And can you use those similarities to do more things with  _it_?
-- Finally, can you teach _it_ quickly to newcomers?
--
-- E.g. do I understand a multi-objective semi-supervised explanation algorithms?
-- Well, Let's check. 
--
-- Here's all that, most of which is coded in B(Ai)TTERY
-- that could be used for other learners.  
-- 
-- Also included here is literate programming,
-- self-documenting code and support for test-driven development.
-- All in around 500 lines of LUA: <br>
--
-- `awk '!/^(--|[ \t]*$)/{n++}`     
-- `END {print n" lines"}' *.lua`  
-- => 500 lines
--      
-- Share and enjoy.
--
--
-- ### Role Models
-- People that inspire me to code less, but better:<br>
-- [Jack Diederich](https://www.youtube.com/watch?v=o9pEzgHorH0), [Hilary Mason](https://www.youtube.com/watch?v=l2btv0yUPNQ),
-- [Brian McFee](https://brianmcfee.net/papers/ismir2011_sptree.pdf),  
-- [Brian Kernighan](https://www.oreilly.com/library/view/beautiful-code/9780596510046/ch01.html),
-- [Joel Grus](https://github.com/joelgrus/data-science-from-scratch).<p>
-- Especially the LISPers: <br>
-- ([Peter Seibel](https://gigamonkeys.com/book/)
--   ([Conrad Barski](https://doc.lagout.org/programmation/Lisp/Land%20of%20Lisp_%20Learn%20to%20Program%20in%20Lisp%2C%20One%20Game%20at%20a%20Time%20%5BBarski%202010-11-15%5D.pdf)
--   ([Paul Graham](http://www.paulgraham.com/onlisp.html)<br>
--     ([Peter Norvig](http://norvig.com/lispy.html)
--       ([Guy Steele](https://dspace.mit.edu/bitstream/handle/1721.1/5790/AIM-353.pdf?sequence=2&isAllowed=y)))))).
