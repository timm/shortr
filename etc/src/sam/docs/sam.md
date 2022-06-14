<span id="forkongithub"><a href="https://github.com/timm/shortr">Fork me on GitHub</a></span>
<a href="https://github.com/timm/shortr/actions/workflows/tests.yml"><img src="https://github.com/timm/shortr/actions/workflows/tests.yml/badge.svg"></a><br><a 
href="https://zenodo.org/badge/latestdoi/206205826"> <img src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a>

<p> <img width=150 align=left src="https://raw.githubusercontent.com/timm/shortr/master/docs/img/cup.png">
<b>Michelangelo:</b><br>
<em>Every <strike>block of stone</strike> program  has a <strike>statue</strike> smaller program inside it and it is the task of the 
<strike>sculptor</strike>designer to discover it.</em>

<b>Dieter Rans:</b><br><em>Less, but better.</em>

If I really know "it", then should I  know all "it"s shortcuts; i.e. how
to write "it" succinctly (in fewer lines) and how to make "it" run faster.
Further, I should be able to generalize "it" such that I can quickly
adapt "it" to other purposes.

So, do I understand a multi-objective semi-supervised explanation?
Well, maybe. Here's all that, most of which is
background stuff that could be used for other learners.  

Once
I build that, I found that decision trees, Naive Bayes classifiers,
and nearest neighbors were all a tiny extension. 

Also included here
is literate programming, self-documenting code and support for
test-driven development. 

All in around 300 lines of LUA: <br>

`awk '!/^(--|[ \t]*$)/ {n++}`   
`END {print n" lines"}' *.lua`  
=> 301 lines
     
Share and enjoy.

## Coding conventions 
- Classes:
  - Classes names in camel case, starting with an Upper case char. 
  - Class methods are shown as Class.UPPERCASE (e.g. Module.NEW for constructors).
  - Instance methods are shown as Class.lowercase(i,...).
  - Instead of using `self`, this code uses `i` and `j` and `k` to denote instances.
- Type hints:
  - Type hints are defined in terms of class names or  int,real, str,tab,bool.
  -  ?type denotes optional arguments.
- Formatting:
  - Any line that just contains `end` is joined to the last line.
  - Where possible, write functions in one-line.
  - Line width max=80 (soft limit) or 83 (hard limit). 
  - Tabs done with 2 spaces.
- Other:
  - Common LUA routines imported from `lib.lua`.
  - Test suite at end (see `Go`).
  - Initial help string defines the config options.
- Command-line options
  - If called as a main program, then `--xxxx value` on the command line updates option `xxxx`.
  - Shorthand #1: `-x`  is the same as  `--xxxx`.
  - Shorthand #2: Boolean `option`s don't use `value`.
    Merely mentioning it on the command-line will flip its default.
