import java.util.concurrent.atomic.AtomicInteger
import ox.cads.locks.Lock
import scala.util.Random
import ox.cads.util.ThreadUtil


// 2a
// for ease we are assuming integers
// we need to use synchronzied blocks so that a single thread can test if there is enough money to withdraw and then make the withdrawal
// this cannot be done in a single atomic action
object SavingsAccount{
	// concurrency stuff
	val lock = Lock()
	val moneyAdded = lock.newCondition

	// NOTE: not sure we even need an atomic integer here since we are using the lock's mutex
	var total = new AtomicInteger(0) // the current amount in the account
	
	def deposit(amount: Int) : Unit = lock.mutex{
		// add the amount 
		total.addAndGet(amount)

		println(total)

		// signal to any threads waitng to withdraw that more money has been added
		moneyAdded.signalAll
		println(total.get.toString)
	}

	// will wait until the account holds at least amount
	// if successful, we withdraw
	def withdraw(amount: Int) : Unit = lock.mutex{
		moneyAdded.await(total.get >= amount)
		total.addAndGet((-1)*amount)

		println(total.get.toString)		
	}

	// test example
	def worker(id: Int) : Unit = {
		val rand = new Random()

		if (id == 0 || id % 2 == 0){ // add money
			while(true){
				var amount = rand.nextInt(10)
				println(s"Thread $id trying to deposit $amount")
				deposit(amount)
			}
		}

		else{ // take money
			while(true){
				var amount = rand.nextInt(100)
				println(s"Thread $id trying to withdraw $amount")
				withdraw(amount)
			}
		}
	}

	def main(args: Array[String]) : Unit = {
		ThreadUtil.runIndexedSystem(4, worker) 
	}
}

// 2b 
object SavingsAccountPreference{
	val lock = Lock()
	val ordinaryRequest = lock.newCondition
	val preferredRequest = lock.newCondition

	var total = new AtomicInteger(0)

	var pendingPreferred = new AtomicInteger(0) // this will count the number of pending preferred withdrawals

	def deposit(amount: Int) : Unit = lock.mutex{
		total.addAndGet(amount)

		// if pending preferred, release
		if (pendingPreferred.get > 0){
			preferredRequest.signalAll
		}

		else{
			ordinaryRequest.signalAll
		}
	}

	def preferredWithdraw(amount: Int) : Unit = lock.mutex{
		// register the preferred request
		pendingPreferred.incrementAndGet()

		preferredRequest.await(total.get >= amount)
		total.addAndGet((-1)*amount)	// decrease total

		var pending_left = pendingPreferred.addAndGet(-1) // decrement the amount pending 

		if (pending_left > 0){ // if there is pending preferred, do nothing - they will have been woken by a deposit signallAll
			()
		}

		else{ // no pending preferred ones, so wake the ordinary ones (if any) 
			ordinaryRequest.signalAll
		}
	}

	def ordinaryWithdraw(amount: Int) : Unit = lock.mutex{
		ordinaryRequest.await(total.get >= amount)
		total.addAndGet((-1)*amount)
	}
}

/*
2c

The lock must be reentrant as to execute transfer, first we must acquire the lock (since the method is wrapped in lock.mutex).
We then execute withdraw(amount) - this acts on the same object, and is also wrapped in lock.mutex (referring to the same lock).
Therefore, if the lock is not reentrant, we will deadlock here, as we will try to acquire a lock that we arleady hold - this will therefor only work if we allow this, which is the definition of a reentrant lock


2d
The current issue is as follows.
Let's say that we want to transfer X money to some other account, but we have less than X currently in our account - we would like this to essentially wait until we have at least X in our account, deposited by some other thread.
Our thread acquires the mutex, calls withdraw(amount) and then waits on a condition.
Let's say that another thread then tries to deposit in our account. We note, that when we awaited inside the withdraw, this released the withdraw mutex, but did not release the 
mutex for transfer (they are of the same lock, each await decreases the hold counter by 1, and another thread can only acquire it if this counter is 0). This causes other threads
to not be bale to acquire the lock - this means no one can ever actually deposit or withdraw anything, and so we reach a deadlock.
*/