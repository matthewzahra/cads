// concurrent implementation of the Sieve

import java.util.concurrent.atomic.AtomicIntegerArray
import java.util.concurrent.atomic.AtomicInteger
import ox.cads.util.ThreadUtil

object ConcurrentSieve{

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
				current.set(id, next.getAndIncrement())
				var n = current.get(id)

				// first we need to check if any of the other threads are working on any numbers that we care about 
				for (i <- 0 to nWorkers - 1){
					var threadCandidate: Long = current.get(i) 
					while (threadCandidate != 0 && threadCandidate*threadCandidate <= n){
						threadCandidate = current.get(i)
						() // spin 
					}
				}

				// test if n is prime 
				var i = 0; var p: Long = primes.get(i)
				while(p != 0 && p*p<=n && n%p != 0){ 
					i += 1
					p = primes.get(i)
				}
				if(p*p > n || p==0){ // n is prime
					// need to try and slot it into the primes array
					// iterate through the array until we either enocunter a 0 (we are at the end) or a number that is larger (it is in the wrong place)
					// NOTE: we start from i found above 
					while(i<N){
						// just set it if we are at the end 
						if(primes.compareAndSet(i,0,n)){
							primesFound.incrementAndGet()
							i=N
						}
						else{
							var num = primes.get(i)
							// if num is too large, we should try and replace it with n and then continue
							if (num > n){
								var done = false // use this to keep trying
								while(!done){
									if (primes.compareAndSet(i,num,n)){ // successfully slotted n in
										current.set(id, num)
										n = num 
										done = true
									}
									else{
										num = primes.get(i) // if statement failed, another thread beat us to it so we need the new value to shift up 
									}
								}
							}
							else{ // number smaller than n, so just continue
								i += 1
							}
						}
					}
				}
				// NEED to clear our entry in current 
				current.set(id, 0)
			}
		}


		ThreadUtil.runIndexedSystem(nWorkers, worker) // spawn threads
		
		println(primes.get(N-1))
    println("Time taken: "+(java.lang.System.currentTimeMillis()-t0))
	}
}