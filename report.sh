for x in grammars/*.gbnf;
do echo $x;
   dune exec bin/main.exe  $x > $x.out  2>&1 || echo err $x
done
echo now report on the most common state
grep -h -C3 error  grammars/*.out|grep State  |sort |uniq -c |sort -n

echo now report on the last lines
tail -n 3  grammars/*.out | grep Look |cut -d " " -f5 | sort |uniq -c |sort -n
     #  1 BAR
     #  2 DASH
     #  7 EOF
     # 16 NEWLINE
echo now report on the last lines


echo now one the last states
tail -n 3  grammars/*.out | grep ^State  | sort |uniq -c |sort -n

echo with filename
for x in grammars/*.out; do
    grep -H ^State $x | tail -1 | grep -v "State 39:"| grep -v "State 43:";
    grep -H Look  $x  | tail -1 |grep -v EOF;
done
