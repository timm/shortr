(gawk 'length($0)==0 {exit} {print $0} ' README.md
 gawk 'length($0)==0 {In=1} In {print $0}' $1) > tmp

mv tmp $1
