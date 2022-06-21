for i in *.lua; do
  if [ "README.md" -nt "$i" ]; then
     echo "# [$i] ..."
     (gawk 'length($0)==0 {exit} {print $0} ' README.md
      gawk 'length($0)==0 {In=1} In {print $0}' $i) > tmp
     mv tmp $i
  fi
done
