import ox.cads.locks._
import scala.reflect.ClassTag

/** A queue using an explicit lock with Condition variables */
class LockedQueue[T : ClassTag](capacity: Int){
  private val lock = new SimpleDelayLock
  private val notFull = lock.newCondition
  private val notEmpty = lock.newCondition

  private val items = new Array[T](capacity)
  var tail = 0; var head = 0; var count = 0
  // This represents:
  //   [ items(i) | i <- [head..tail) ],  
  //        if head < tail || count == 0
  //   [ items(i) | i <- [head..capacity)++[0..tail) ],
  //        if head > tail || count == capacity
  // count = tail-head or count = tail-head+capacity; 0 <= count <= capacity

  /** Enqueue x. */ 
  def enqueue(x: T) = lock.mutex{
    notFull.await(count < capacity)
    items(tail) = x; count += 1
    tail += 1; if(tail == capacity) tail = 0 // enqueue item
    notEmpty.signal // wake up any blocked dequeue
  }

  /** Dequeue a value. */
  def dequeue : T = lock.mutex{
    notEmpty.await(count != 0)
    // while(count == 0) notEmpty.await // wait until not empty
    val x = items(head); count -= 1 
    head += 1; if(head == capacity) head = 0 // dequeue item
    notFull.signal; x // wake up any blocked enqueue
  }
}

@main def hello(): Unit = 
    println("lol")
    val x = new SimpleDelayLock