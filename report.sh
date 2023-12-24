for x in grammars/*.gbnf;
do echo $x;
   dune exec bin/main.exe  $x > $x.out	 2>&1 || echo err $x
done 
grep -h -C3 error  grammars/*.out|grep State  |sort |uniq -c |sort -n
