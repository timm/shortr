<a name=top></a>

<img align=right width=650 src=/docs/img/contrib.png>

# Contribute
Hello :wave:.  

Contributions welcome :kiss: :heart:.

If you offer pull requests to this code, please use these conventions.

## Classes:
- Classes names in camel case, starting with an Upper case char. 
- Class methods are shown as Class.UPPERCASE (e.g. Module.NEW for constructors).
- Instance methods are shown as Class.lowercase(i,...).
- Instead of using `self`, this code uses `i` and `j` and `k` to denote instances.

## Type hints:
- Type hints are defined in terms of class names or  int,real, str,tab,bool.
-  ?type denotes optional arguments.

## Formatting:
- Any line that just contains `end` is joined to the last line.
- Where possible, write functions in one-line.
- Line width max=80 (soft limit) or 83 (hard limit). 
- Tabs done with 2 spaces.

## Other:
- Common LUA routines imported from `lib.lua`.
- Test suite at end (see `Go`).
- Initial help string defines the config options.

## Command-line options
- If called as a main program, then `--xxxx value` on the command line updates option `xxxx`.
- Shorthand #1: `-x`  is the same as  `--xxxx`.
- Shorthand #2: Boolean `option`s don't use `value`.
  Merely mentioning it on the command-line will flip its default.
