/** Sudoku solver.
  * Basic usage: 
  * # scala Sudoku filename: solves the puzzle in filename
  * # scala Sudoku --all: solves all the puzzles in this directory
  * # scala Sudoku --allPoss: solves all the possible puzzles in this directory
  * Options:
  * # -n n: repeat n times
  * # -a: use the AdvancedPartial
  */

import ox.cads.util.Profiler
import ox.cads.collection.LockFreeStack
import ox.cads.util.ThreadUtil
import java.util.concurrent.atomic.AtomicBoolean
import ox.cads.collection.Pool
import ox.cads.collection.TerminationDetectingPool

// code to adapt teh LockFreeStack so that it can count as Pool
class LockFreeStackPool[T] extends Pool[T]{
  val stack = new LockFreeStack[T]

  def add(x: T) : Unit = {
    stack.push(x)
  }

  def get : Option[T] = {
    stack.pop
  }
}

object SudokuConcurrent{
  var n_workers = 20

  // Stack to store partial solutions that we might back-track to.
  // global to each object so all threads can access it 
  var actual_stack = new LockFreeStackPool[Partial]
  var stack = new TerminationDetectingPool(actual_stack, n_workers)
  var done = new AtomicBoolean(false) // going to be shared by all threads - will signal when we are actually done 



  // initialise the stack with a partial
  def initialise_stack(init: Partial) : Unit = {
    stack.add(init)
  }

  // reset the stack and done for a new solve attempt
  def reset(): Unit = {
    actual_stack = new LockFreeStackPool[Partial]
    stack = new TerminationDetectingPool(actual_stack, n_workers)
    done.set(false)
  }

  /** Solve the puzzle defined by init */
  def solve(): Unit = {
    while(!done.get()){
      val stack_result = stack.get

      stack_result match
        case None => return // stack is empty so we terminate
        case Some(partial) =>
          if(partial.complete){  // done!
            if(!done.getAndSet(true)){
              partial.printPartial 	// TODO - make sure only 1 thread prints this 
              stack.signalDone 			// tell all the threads on the queue to terminate 
            }
          }
          else{
              // Choose position to play
              val(i,j) = partial.nextPos;
              // Consider all values to play there
              for(d <- 1 to 9)
              if(partial.canPlay(i,j,d)){
                  val p1 = partial.play(i,j,d); stack.add(p1)
              }
          }
    } // end of while
  }

  /** A list of files containing possible puzzles */
  private val allPossibleFiles = 
    List("test0.sud", "test1.sud", "test2.sud", "test3.sud", "test4.sud", 
    "test5.sud", "test6.sud", "test7.sud", "test8.sud", "test9.sud")
  /** A list of files containing puzzles, including one impossible one. */
  private val allFiles = allPossibleFiles ++ List("impossible.sud")

  def main(args:Array[String]) = {
    val t0 = System.currentTimeMillis()

    // options
    var count = 1 // number of tests
    var fname = "" // filename
    var adv = false // are we using the AdvancedPartial?
    var all = false // are we doing all files in allFiles?
    var allPoss = false // are we doing all files in allPossibleFiles?
    // parse command line arguments
    var i = 0
    while(i < args.length){
      if (args(i)=="-n"){ count = args(i+1).toInt; i+=2 }
      else if (args(i)=="-a"){ adv = true; i+=1 }
      else if (args(i) == "--all"){ all = true; i += 1 }
      else if (args(i) == "--allPoss"){ allPoss = true; i += 1 }
      else{ fname = args(i); i += 1 }
    }
    assert(all || allPoss || fname != "")

    // Initialise partial from file fname
    def mkPartial(fname: String) = {
      val partial = if(adv) new AdvancedPartial else new SimplePartial
      partial.init(fname)
      partial
    }

    // what we want each thread to run 
    def worker(target: Partial): Unit = {
      solve()
    }

    // Solve count times
    // for(i <- 0 until count)
    //   if(all) for(f <- allFiles){ println(f); solve(mkPartial(f)) }
    //   else if(allPoss) 
    //   for(f <- allPossibleFiles){ println(f); solve(mkPartial(f)) }
    //       else solve(mkPartial(fname))

    // just going to solve all the files (except impossible) for now 
    for(f <- allPossibleFiles){
      println(f)
      initialise_stack(mkPartial(f))
      ThreadUtil.runSystem(n_workers, solve())
      reset()
    }

    println("Time taken: "+(System.currentTimeMillis()-t0))
    Profiler.report
  }    
}

// TODO - currently i think that if the stack is empty but things are pending, it returns None to some?
// need to ensure that threads spin if we haven't finished instead of returning None - 
// check implementation of LockFreeStackPool