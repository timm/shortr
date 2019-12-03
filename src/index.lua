-- vim: ts=2 sw=2 sts=2 expandtab:cindent:
------- --------- --------- --------- --------- --------- 

-- <img src="https://img.shields.io/badge/language-lua-orange">
-- <img src="https://img.shields.io/badge/purpose-ai,se,teaching-blueviolet">
-- <img src="https://img.shields.io/badge/platform-mac,linux-informational">
-- <img src="https://travis-ci.org/timm/lua.svg?branch=master">

-- ( Just a little light LUA. Less, but better. )

-- <img  width=400 
--  src="https://github.com/timm/lua/raw/master/etc/img/index.jpg">


-- Why combine data miners and optimisers? Well, consider:
--
-- - Data miners divide up some space of things. 
-- - Optimizers suggest ways to move in a space.
-- - So, in short, both approaches are ways to model and understand that space.

-- So data miners and optimizers are two sides of the same coin.
-- By refactoring and combining both approaches, we can find simple ways to (e.g.) optimize
-- with fewer samples to that space or (e.g.) build classifiers or regression tools
-- that satisfy
-- multiple objectives. 
--
-- Any why code this in LUA?
-- 
-- - LUA is  a wonderful language: clean and simple syntax, quick to learn, very portable, supports
-- garbage collection, objects, functional programming, and tail call optimization. Also, it is
-- well documented, and can boast a friendly and enthusiastic  support community.
-- - When you need AI on edge devices,
-- better to use something very concise and lightweight (like LUA).
-- - Finally, LUA is very useful  for training graduate students in how to build AI systems ("Here, take this executable
-- specification and code it up in e.g. Python"). 
--
-- ## Contents
