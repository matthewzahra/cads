// concurrent implementation of the Sieve

import java.util.concurrent.atomic.AtomicIntegerArray
import java.util.concurrent.atomic.AtomicInteger
import ox.cads.util.ThreadUtil

object ConcurentSieve{

	def main(args: Array[String]) = {
		assert(args.length == 1, "must have one argument")
    val t0 = java.lang.System.currentTimeMillis()

    val N = args(0).toInt // number of primes required
    val primes = new AtomicIntegerArray(N) // will hold the primes
    primes.set(0,2)
    var nextSlot = 1 // next free slot in primes
    var next = new AtomicInteger(3) // next candidate prime to consider

		val nWorkers = 4 // define the number of worker threads
		val current = new AtomicIntegerArray(nWorkers)	// stores the numbers that each thread is currently working on - NOTE: values of 0 will be interpreted as that thread not working on any value

		var primesFound = new AtomicInteger(1) // counts how many prime numbers we have found 

		// define what worker threads need to do 
		def worker(id: Int) = {
			while(primesFound.get < N){
				// get the next number to check if it is prime and record it in current
				var n = next.getAndIncrement()
				println(n.toString)
				current.set(id, n)	

				// first we need to check if any of the other threads are working on any numbers that we care about 
				for (i <- 0 to nWorkers - 1){
					while (current.get(i) != 0 && current.get(i)*current.get(i) <= n){
						() // spin 
					}
				}

				// test if n is prime 
				var i = 0; var p = primes.get(i)
				while(p*p<=n && n%p != 0){ 
					i += 1
					p = primes.get(i)
				}
				if(p*p > n){ // n is prime
					// need to try and slot it into the primes array
					// iterate through the array until we either enocunter a 0 (we are at the end) or a number that is larger (it is in the wrong place)
					var i = 0
					while(i<N){
						if(primes.get(i) == 0){ // at the end, so just slot in and move on with our lives
							primes.set(i, n)
							primesFound.incrementAndGet() // TODO - is this allowed?
							i = N
						}

						else if(primes.get(i) > n){ // need to move numbers around
							// set number that is too large with n, put n where it is and put new number in current 
							var tooLarge = primes.get(i)
							primes.set(i,n)
							n = tooLarge
							current.set(id, n)
						}

						else{ // just increment counter
							i+=1
						}
					}

					// NEED to clear our entry in current 
					current.set(id, 0)
				}
			}
		}


		ThreadUtil.runIndexedSystem(nWorkers, worker)
		
		println(primes.toString)
    println("Time taken: "+(java.lang.System.currentTimeMillis()-t0))
	}



}

// to maintain order of primes, when we have found a prime we need to find where t oslot it in, and swap with something in wrong place
// this is fine as we are only moving larger values around, and thus no thread will need them at the time
// ensure that any values we take out are then stored in current 