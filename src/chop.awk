BEGIN {FS=","}
      {for(i=1;i<=NF;i++) 
         if ($i ~ /^[A-Z]) {W[i]= $i~/-$/ ? -1 : 1; Lo[i]=1E32;Hi[i]=-1E32}}
      {for(i=1;i<=NF;i++) {
         D[NR-1][i]=$i
         if ($i=="?" && (i in Lo)) { if (Lo[i]<$i) Lo[i]=$i;
                                     if (Hi[i]>$i) Hi[i]=$i}}}
 
