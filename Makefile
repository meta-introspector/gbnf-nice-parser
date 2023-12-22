duneTest:
	dune test --verbose       --force
test1:
	menhir --trace -v --interpret  ./lib/sentenceParser.mly < test/test.gbnf

compile:
	menhir --cmly --table --trace --dump --explain --log-grammar 99 --log-automaton 9 --log-code 99 --log-grammar 99 --reference-graph lib/sentenceParser.mly	
	rm -f lib/sentenceParser.cmly lib/sentenceParser.ml lib/sentenceParser.mli
	dune clean
	dune build
	dot ./lib/sentenceParser.dot -Tpng -o sentenceParser.png  -Gsize=8.5,11 -Gdpi=300 -Grankdir=LR
	dot ./lib/sentenceParser.dot -Tsvg -o sentenceParser.svg -Gsize=8.5,11 -Gdpi=300 -Grankdir=LR

	dune test || echo skip
