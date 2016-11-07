import Test.QuickCheck

-- 1: k+1 steps är being computed

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- 2:
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power1: negative argument"
power1 n k = product (replicate (fromInteger k) n)

--3:
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power2: negative argument"
power2 n 0 = 1
power2 n k | even k = power2 (n*n) (div k 2)
power2 n k | odd k = n * power2 n (k-1)

--4:
--Test motivation: 0 is a base case and should work, -1 generates error (not with prop_powers though), 1 is lowest that is not a base case and should be tested
--5 is odd number, 10 is even number, 51 is pretty high and 10000 is very high. Power should work for all these (higher than 10000 is very slow)
--if we have a comprehension list like this we get all types of combinations between these values
testlist = [(x,y) | x<-[0,-1,1,5,10,51,10000], y<-[0,-1,1,5,10,51,10000]]
testlist2 = []

--run test by writing "mytest testlist" in terminal
mytest :: [(Integer, Integer)] -> Bool
mytest []          = True
mytest ((i1,i2):l) = prop_powers i1 i2 && mytest l


prop_powers :: Integer -> Integer -> Bool
prop_powers n k | k < 0 = True --QuickCheck can't handle the error "power: negative argument"
prop_powers n k = power n k == power1 n k && power1 n k == power2 n k
