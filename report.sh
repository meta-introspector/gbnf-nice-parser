for x in grammars/*.gbnf;
do echo $x;
   dune exec bin/main.exe  $x > $x.out	 2>&1 || echo err $x
done
echo now report on the most common state
grep -h -C3 error  grammars/*.out|grep State  |sort |uniq -c |sort -n

echo now report on the last lines
tail -n 3  grammars/*.out | grep Look |cut -d " " -f5 | sort |uniq -c |sort -n
     #  1 BAR
     #  2 DASH
     #  7 EOF
     # 16 NEWLINE

echo now one the last states
tail -n 3  grammars/*.out | grep ^State |cut -d " " -f5 | sort |uniq -c |sort -n
