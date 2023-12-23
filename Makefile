duneTest:
	dune test --verbose       --force
test1:
	menhir --trace -v --interpret  ./lib/sentenceParser.mly < test/test.gbnf

testall:
	for x in grammars/*.gbnf; do echo $x; dune exec bin/main.exe  $x > $x.out 2>&1; done
compile:
	menhir --cmly --table --trace --dump --explain --log-grammar 99 --log-automaton 9 --log-code 99 --log-grammar 99 --reference-graph lib/sentenceParser.mly	
	rm -f lib/sentenceParser.cmly lib/sentenceParser.ml lib/sentenceParser.mli
	dune clean
	dune build
	dot ./lib/sentenceParser.dot -Tpng -Gsize=8.5,11 -Gdpi=300 -Grankdir=LR -Gorientation="[lL]*" -o sentenceParser.png  
	dot ./lib/sentenceParser.dot -Tsvg -Gsize=8.5,11 -Gdpi=300 -Grankdir=LR -Gorientation="[lL]*" -o sentenceParser.svg 

	dune test || echo skip

sentenceParser.png  : ./lib/sentenceParser.dot
	dot ./lib/sentenceParser.dot -Tpng -Gsize=8.5,11 -Gdpi=300 -Grankdir=LR -Gorientation="[lL]*" -o sentenceParser.png  
