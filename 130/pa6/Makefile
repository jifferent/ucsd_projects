FSC = fsc -Djava.io.tmpdir=.scala-devel

all: bst decorators test

bst: 
	$(FSC) Bst.scala

decorators:
	$(FSC) Decorators.scala

json:
	$(FSC) Json.scala

test: bst decorators json 
	$(FSC) Test.scala
	scala Test

clean:
	rm -f *.class *.tmp
	#$(FSC) -reset

distclean: clean
	$(FSC) -shutdown

turnin: 
	zip pa6.zip Json.scala Bst.scala Decorators.scala
	turnin -c cs130s -p pa6 pa6.zip 

zip: 
	zip pa6.zip *.scala *.out Makefile
