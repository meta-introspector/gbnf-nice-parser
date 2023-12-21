test:
	dune clean
	dune build
	dune test --force

sentence_messages.ml: sentenceParser.messages  sentenceParser.mly
	menhir --compile-errors sentenceParser.messages  sentenceParser.mly  > sentence_messages.ml
