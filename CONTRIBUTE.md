<a name=top></a>

<img align=right width=550 src=/docs/img/contrib.png>

# Contribute
Hello :wave:.  

Contributions welcome :kiss: :heart:.

If you offer pull requests to this code, please use these conventions.

## Metatables (just say no)
- Sorry, but please  don't use metatables. Why? Well, the whole point of this code is make people say "hey, that
   is so easy I can write it in Rust, Typescript, Pythons, Erlang,  Ruby, etc, etc...". So this code steers
   clear of clever LUA
   constructs that might not easily port.

## Classes:
- Classes names in camel case, starting with an Upper case char. 
- Class methods are shown as Class.UPPERCASE (e.g. Class.NEW for constructors).
- Instance methods are shown as Class.lowercase(i,...).
- Instead of using `self`, this code uses `i` and `j` and `k` to denote instances.

## Code formatting:
- Any line that just contains `end` is joined to the last line.
- Where possible, write functions in one-line.
- Line width max=80 (soft limit) or 83 (hard limit). 
- Tabs done with 2 spaces.

## Html
- If you want a header on the html version of file `x.lua` then add the file `docs/x.md`.
- Please use the comment idiom `--> thing -> comment` to type hints from comment strings.
  Note that [Makefile](makefile) rule for docs/*.html pretty prints such comment idioms.

## Type hints:
- Type hints are defined in terms of class names or  :int,:real, :str,:tab,:bool.
-  :?type denotes optional arguments.
- :[type] denotes list of type.
- :[[type]] denotes list of list of type.

## Other:
- Common LUA routines imported from `lib.lua`.
- Test suite at end (see `Go`).
- Initial help string defines the config options.

## Command-line options
- If called as a main program, then `--xxxx value` on the command line updates option `xxxx`.
- Shorthand #1: `-x`  is the same as  `--xxxx`.
- Shorthand #2: Boolean `option`s don't use `value`.
  Merely mentioning it on the command-line will flip its default.
