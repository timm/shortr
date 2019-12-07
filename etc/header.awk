BEGIN {
 q="\""
 Title="DUO = Data Miners Using and/or Used-by Optimizers"
 Maths="<script src=${q}https://polyfill.io/v3/polyfill.min.js?features=es6${q}>"\
       "</script><script id=MathJax-script async src="\
       "${q}https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js$}q}></script>"
  Menu="<a href=${q}index.html${q}>home</a> :: "\
       "<a href=${q}https://github.com/timm/lua/tree/master/INSTALL.md>install${q}</a> :: "\
       "<a href=${q}about.html${q}>about</a> :: "\
       "<a href=${q}https://github.com/timm/lua/tree/master/test${q}>demo</a> :: "\
       "<a href=${q}http://github.com/timm/lua>src${q}</a> :: "\
       "<a href=${q}http://github.com/timm/lua/issues>discuss${q}</a> :: "\
       "<a href=${q}https://github.com/timm/lua/blob/master/LICENSE.md${q}>©&nbsp;2020</a> by "\
       "<a href=${q}http://menzies.us${q}>timm</a>"

  Bar="<img src=${q}https://img.shields.io/badge/language-lua-orange${q}> "\
      "<img src=${q}https://img.shields.io/badge/purpose-ai,se-blueviolet${q}> "\
      "<img src=${q}https://img.shields.io/badge/platform-mac,*nux-informational${q}> "\
      "<img src=${q}https://img.shields.io/badge/license-Bsd2-informational${q}> "\
      "<img src=${q}https://travis-ci.org/timm/lua.svg?branch=master${q}>"
}
sub("<h1>",    "<h1>" Title)
sub("</head>",  Maths"</head>")
sub("<p>vi,:.*","<p>"Menu"</p>")

cat<<'EOF'
-- <img src="https://img.shields.io/badge/language-lua-orange">
-- <img src="https://img.shields.io/badge/purpose-ai,se-blueviolet">
-- <img src="https://img.shields.io/badge/platform-mac,*nux-informational">
-- <img src="https://img.shields.io/badge/license-Bsd2-informational">
-- <img src="https://travis-ci.org/timm/lua.svg?branch=master">
EOF

-- <img src="https://img.shields.io/badge/language-lua-orange">
-- <img src="https://img.shields.io/badge/purpose-ai,se-blueviolet">
-- <img src="https://img.shields.io/badge/platform-mac,*nux-informational">
-- <img src="https://img.shields.io/badge/license-Bsd2-informational">
-- <img src="https://travis-ci.org/timm/lua.svg?branch=master">

