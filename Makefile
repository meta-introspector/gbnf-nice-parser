duneTest:
	dune test --verbose       --force
test1:
	menhir --trace -v --interpret  ./lib/sentenceParser.mly < test/test.gbnf
