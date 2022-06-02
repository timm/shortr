# Contribute 
Hello :wave:  

Contributions welcome :kiss: :heart:


If you offer pull requests to this code, please follow the following conventions.

**LUA conventions** 
- Minimize use of local
  -  Define all local names at top of file (so code can be defined in any order).
- Use `i` instead of `self`. 
- When definining methods:
  - Don't use `function FRED:method() return self.n + 1 end`;
  - Do use `function FRED.method(i) return i.n+1 end`.
- Suggestions for variable names
  - Use `col` for instances of NUM and SYM class
  - Use `v` for cells in rows
- Layout lines 80 chars side (max,ish). Use 2 spaces for "tab".
- Do functions as one-liners (if possible). Multi-line functions need a trailing


**Objects**: yes,  inheritance, no (harder to debug)   
- UPPERCASE functions are constructors

**Tests*: Test cases stored in `go` functions.    
- Tests should return `true` if the test passes or a warning string if otherwise
- Tests should be silent unless they fail or the verbose flag is set in command line.
- Tests can be disabled by renaming from `go.fun` to `no.fun`.
- All tests run at run time (exception, if `-g xx` appears on command-line, just run 
  `go.xx()`.

**Settings**: Define global settings   
in the help strings at top of file.    
- Allow for `-h` on the command line to print help
- Settings stored in the global "THE"
- Settings generated from "help" string
- Settings can be updated from the strings seed in flags

Beware missing values (marked in "?") and avoid them

**Linting**:  
--  1st line: traps list of pre-existing globals  
-- 2nd last line: look for "rogue" globals (by comparing with data trapped on line1)
-- Last line: exit to operating system with number of failures seen in tests
