-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- ---------

local function interp(s, tab)
  return (s:gsub('($%b{})', function(w) return tab[w:sub(3, -2)] or w end))
end

local function slurp(file, stream)
  local f = io.open(file)
  local s = f:read("*a")
  f:close()
  return s
end

local math="""
<script src="https://polyfill.io/v3/polyfill.min.js?features=es6"></script>
<script id=MathJax-script async 
        src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>
<script type="text/x-mathjax-config">
        MathJax.Hub.Config({
            tex2jax: {
                  inlineMath: [ ['$','$'], ["\\(","\\)"] ],
                  processEscapes: true
            }
        });
</script>
"""

local gh="""
<a href="https://github.com/timm/lua/src"><img 
  width="149" height="149" 
  src="https://github.blog/wp-content/uploads/2008/12/forkme_right_red_aa0000.png?resize=149%2C149" 
  class="attachment-full size-full" 
  alt="Fork me on GitHub" 
  data-recalc-dims="1"></a>
"""

local template=
"""
<a href="index.html">home</a> :: 
<a href="about.html">about</a> :: 
<a href="https://github.com/timm/lua/tree/master/INSTALL.md>install"</a> :: 
<a href="https://github.com/timm/lua/tree/master/test">demos</a> :: 
<a href="http://github.com/timm/lua/issues>discuss"</a> by 
<a href="http://menzies.us">timm</a><br>"
"""
--------- --------- --------- --------- --------- ---------

# DUO =  Data Miners using and/or used-by Optimizers

# {title}

--------- --------- --------- --------- --------- ---------
<img src=https://img.shields.io/badge/language-lua-orange> 
<img src=https://img.shields.io/badge/purpose-ai,se-blueviolet> 
<img src=https://img.shields.io/badge/platform-mac,*nux-informational> 
<a href="https://github.com/timm/lua/blob/master/LICENSE.md"><img
   src=https://img.shields.io/badge/license-Bsd2_2020-informational></a> 
<a href="https://travis-ci.org/timm/lua"><img 
   src=https://travis-ci.org/timm/lua.svg?branch=master></a>
"
