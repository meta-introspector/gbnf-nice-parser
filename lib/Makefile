compile:
	menhir --cmly --table --trace --dump --explain --log-grammar 99 --log-automaton 9 --log-code 99 --log-grammar 99 --reference-graph sentenceParser.mly	
	rm -f sentenceParser.cmly sentenceParser.ml sentenceParser.mli
	dune clean
	dune build


sentence_messages.ml: sentenceParser.messages  sentenceParser.mly
	menhir --compile-errors sentenceParser.messages  sentenceParser.mly  > sentence_messages.ml
