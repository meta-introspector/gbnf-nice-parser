duneTest:
	dune test --verbose       --force
test1:
	menhir --trace -v --interpret  ./lib/sentenceParser.mly < test/test.gbnf

compile:
	menhir --cmly --table --trace --dump --explain --log-grammar 99 --log-automaton 9 --log-code 99 --log-grammar 99 --reference-graph lib/sentenceParser.mly
