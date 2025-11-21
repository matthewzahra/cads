import scala.reflect.ClassTag
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicReferenceArray
import java.util.concurrent.atomic.AtomicInteger

// lock free version of the queue

class LFArrayListQueue[T: ClassTag] extends ox.cads.collection.Queue[T]{
	val size = 10

	// node class that holds multiple data items
	private class Node{
		val data = new AtomicReferenceArray[Option[T]](size) // NOTE - initially these are filled with null values 
		var next = new AtomicReference[Option[Node]](None)
		var head = new AtomicInteger(0)
		var tail = new AtomicInteger(0)
	}
	private var firstNode = new Node()
	@volatile private var head = new AtomicReference(firstNode) // dummy header
	@volatile private var tail = new AtomicReference(firstNode) // last Node in list

	def enqueue(x: T) = {
		var done = false //we must keep trying until we succeed

		while (!done){
			var node = tail.get
			var nodeTail = node.tail.get

			if (nodeTail == size){ // node is full
				var newNode = new Node()
			
				// need to update its pointer and need to update tail 
				// update pointer first
				node.next.compareAndSet(None,Some(newNode)) // try update the next pointer
				
				// update tail (if not already done) - tail may temporarily be the penultimate node
				var tailNext = tail.get.next.get
				tailNext match{
					case None => ()
					case Some(n) => tail.compareAndSet(node, n)
				}
			}

			else{
				// get the node.tail value and check if the slot it corresponds to is empty (i.e. nothing has been put in it)
				if (node.data.get(nodeTail) == null) { // there is room to put our item in 
					if (node.data.compareAndSet(nodeTail, null, Some(x))){
						done = true // we managed to enqueue our item, so we are done and can exit 
					} 

					node.tail.compareAndSet(nodeTail, nodeTail+1) // try and increment the tail pointer for this node
				}

				else{ // someone has already slotted something in here - we need to try and increment the tail pointer just incase they get descheduled
					node.tail.compareAndSet(nodeTail, nodeTail+1)
				}
			}
		}
	}


	def dequeue : Option[T] = {
		var done = false 
		var result: Option[T] = None

		while (!done){
			var node = head.get
			var nodeHead = node.head.get
			var nodeTail = node.tail.get

			if (nodeHead == nodeTail && nodeHead < size){ // node is empty but not full
				done = true
			}

			else{
				if (nodeHead == size){ // node is empty and full - get the next one if it exists
					var nextNode = node.next.get
					nextNode match{
						case None => done = true // no next node yet so we are empty
						case Some(n) => {
							// try and progress head
							head.compareAndSet(node,n)
						}
					}
				}
				else{ // potentially not empty - try and increase head pointer in node to "reserve" our item
					if (node.head.compareAndSet(nodeHead, nodeHead+1)){
						result = node.data.get(nodeHead)
						done = true
					}
				}
			}
		}
		return result
	}
}