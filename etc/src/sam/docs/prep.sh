for i in *.md; do
  if [[ "README.md" -nt "$i" ]]; then
     echo "# $i ..."
     (gawk 'length($0)==0 {exit} {print $0} ' README.md
      gawk 'length($0)==0 {In=1} In {print $0}' $i) > tmp
     mv tmp $i
  fi
  f=../src/${i/.*/}.lua
  if [[ "$i" -nt "$f" ]]; then
     echo "# $f ..."
     cat $i > $f 
  fi 
done
