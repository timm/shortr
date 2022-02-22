clear
date +'%M : %S' | figlet -f small
lua sl.lua -todo all | gawk '
BEGIN   { fails=0 }
/FAIL/  { ++fails }
        { sub(/PASS/,"\033[1;32mPASS\033[0m")
          sub(/TASK/,"\033[1;34mTASK\033[0m")
          sub(/FILE/,"\033[1;36mFILE\033[0m")
          sub(/FAIL/,"\033[1;31mFAIL\033[0m")
          print }
END     { print("\n==> \033[1;33m"fails"\033[0m failures")
          exit(fails) }'

