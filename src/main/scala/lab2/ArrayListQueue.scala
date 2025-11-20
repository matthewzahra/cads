import scala.reflect.ClassTag
import ox.cads.locks.Lock

class ArrayListQueue[T: ClassTag] extends ox.cads.collection.Queue[T]{
	val size = 10

	// node class that holds multiple data items
	private class Node{
		val data: Array[T] = new Array[T](size)
		var next: Option[Node] = None 
		var head: Int = 0
		var tail: Int = 0
	}

	val enqLock = Lock()
	val deqLock = Lock()

	private var head = new Node() // dummy header
	private var tail = head // last Node in listj


	def enqueue(x: T) = enqLock.mutex{
		var node = tail
		if (node.tail == size){ // node full, add a new node
			var newNode = new Node()
			node.next = Some(newNode)
			node = newNode
			tail = node	// udpate the tail
		}
		// add data to node
		node.data(node.tail) = x

		// increment tail
		node.tail += 1
	}

	def dequeue: Option[T] = deqLock.mutex{
		var node = head

		if (node.head == node.tail){ // head and tail at same place, get next node if there is one 
			node.next match{
				case None => return None
				case Some(n) => {
					node = n
					head = node
				}
			}
			
		}

		if (node.head == 0){ // node is empty
			if (node == tail){
				return None
			} // whole list is empty
			
			// get next node 
			node.next match{
				case None => throw new Exception("oh no...")
				case Some(n) =>{
					node = n
					head = node
				}
			}
	
		}

		var item = node.data(node.head)
		node.head += 1

		return Some(item)
	}
}