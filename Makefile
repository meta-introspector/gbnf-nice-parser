grammars/c.gbnf.out: grammars/c.gbnf
	dune exec bin/main.exe  $< | tee  $>.out	 2>&1 
grammars/segfault.gbnf.out: grammars/segfault.gbnf
	dune exec bin/main.exe  $< | tee  $>.out	 2>&1 

grammars/tools.gbnf.out: grammars/tools.gbnf
	dune exec bin/main.exe  $< | tee  $>.out	 2>&1 

grammars/json.gbnf.out: grammars/json.gbnf
	dune exec bin/main.exe  $< | tee  $>.out	 2>&1 
grammars/hazel.gbnf.out: grammars/hazel.gbnf
	dune exec bin/main.exe  $< | tee  $>.out	 2>&1 
grammars/gallina2.gbnf.out: grammars/gallina2.gbnf
	dune exec bin/main.exe  $< | tee  $>.out	 2>&1 
grammars/gallina3.gbnf.out: grammars/gallina3.gbnf
	dune exec bin/main.exe  $< | tee  $>.out	 2>&1 

all :testall
	echo ok

log:
	bash ./build.sh

duneTest:
	dune test --verbose       --force
test1:
	menhir --trace -v --interpret  ./lib/sentenceParser.mly < test/test.gbnf

testall: compile
	bash ./report.sh

runmenhir: lib/sentenceParser.mly	
	menhir --cmly --table --trace --dump --explain --log-grammar 99 --log-automaton 9 --log-code 99 --log-grammar 99 --reference-graph lib/sentenceParser.mly
	rm -f lib/sentenceParser.cmly lib/sentenceParser.ml lib/sentenceParser.mli
compile: runmenhir
	dune clean
	dune build
	dot ./lib/sentenceParser.dot -Tpng -Gsize=8.5,11 -Gdpi=300 -Grankdir=LR -Gorientation="[lL]*" -o sentenceParser.png  
	dot ./lib/sentenceParser.dot -Tsvg -Gsize=8.5,11 -Gdpi=300 -Grankdir=LR -Gorientation="[lL]*" -o sentenceParser.svg 

	dune test || echo skip

sentenceParser.png  : ./lib/sentenceParser.dot
	dot ./lib/sentenceParser.dot -Tpng -Gsize=8.5,11 -Gdpi=300 -Grankdir=LR -Gorientation="[lL]*" -o sentenceParser.png  
