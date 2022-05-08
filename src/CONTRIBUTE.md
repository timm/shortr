minimize use of local (exception: define all functions as local at top of file)

no inheritance

UPPERCASE functions are constructors
lowercase functions of same name do updates

Use `i` instead of `self`.

`go` functions store tests. tests should be silent unless they fail
tests can be disabled by renaming from `go.fun` to `no.fun`.

tests should return `true` if the test passes
or a warning string if otherwise

code 80 chars wide, or less

functions in 1 line, if you can.

set flags in help string top of file. allow for `-h` on the command line
to print help

beware missing values (marked in "?") and avoid them

learning
-- where possible, fully incremental
-- isolate operating system interaction
