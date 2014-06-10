ones = 1:ones
nats = 1:(map (+ 1) nats)
fibs = 1:1:(zipWith (+) fibs (tail fibs))
primes = sieve (tail nats)
  where
    sieve (x:xs) = x:(sieve (filter (\y -> (mod y x) /= 0) xs))
