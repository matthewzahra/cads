COMPONENTS = Partial.class SimplePartial.class AdvancedPartial.class	\
Sudoku.class # ConcurrentSudoku.class

all: $(COMPONENTS)

%.class: %.scala
	 scalac -classpath /home/matthewzahra/coding/scala/lib *.scala

run:
	scala-cli run Partial.scala SimplePartial.scala AdvancedPartial.scala Sudoku.scala --classpath /home/matthewzahra/coding/scala/lib -- --all

run_concurrent:
	scala-cli run Partial.scala SimplePartial.scala AdvancedPartial.scala SudokuConcurrent.scala --classpath /home/matthewzahra/coding/scala/lib -- --all


clean:
	rm -f *.class
	rm -f *.tasty
