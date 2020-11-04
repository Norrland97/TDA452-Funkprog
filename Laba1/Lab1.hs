import Test.QuickCheck
{- Lab 1
   Date: 2020-11-03
   Authors: Viktor Fredholm, Aline Eikeland
   Lab group: 25
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k+1 


-- B -------------------------
-- power1


power1 :: Integer -> Integer -> Integer
power1 n k  = product (listify n k)
   
listify :: Integer -> Integer -> [Integer]
listify n k | k == 0 = [1]
            | k > 0  = ([n] ++ [power1 n (k-1)])
            | k < 0  = error "power: negative argument"
       
testPower1 n k = power1 n k == power n k

-- C -------------------------
-- power2

power2 n k  | k==0 = 1
            | k > 0 && isEven= power (n * n) (k `div` 2)
            | k > 0 = n * (power n (k-1))
            | k < 0 = error "power: negative argument"
         where isEven = (k `mod` 2) == 0

testPower2 :: Integer -> Integer -> Bool
testPower2 n k = power2 n k == power n k

-- D -------------------------

prop_powers :: Integer -> Integer -> Bool
prop_powers n k = testPower1 n k == testPower2 n k

prop_powers' n k = testPower1 n (abs k) == testPower2 n (abs k)

printTest bos  | length bos == 0 = []
               | otherwise       = show (head bos) : printTest (tail bos)

powerTest = powerTest' nTestValues kTestValues  

powerTest' :: [Integer] -> [Integer] -> [Bool]
powerTest' ns ks | length ns == 0 = []
                 | otherwise      = prop_powers (head ns) (head ks) : powerTest' (tail ns) (tail ks)

nTestValues :: [Integer]
nTestValues = [3,0,6,0,1,5,-2,10000000,4,2]

kTestValues :: [Integer]
kTestValues = [6,0,0,7,10,0,4,4,20000,-5]

{- 

<Describe your test cases here>
We chose the values to see that some basic values would work, and then to see that negative values and big numbers works.
As the last test value we chose to have a negative k-value, tihs gives an error and that is expected. Catchings errors is not praxis in Haskell, so we instead made sure to put the error inducing value last in the list.
20 000 is chosen as the largest exponent value that took a reasonable amount of time to run.
We made sure to test cases with a base 0, an exponent 0 as well as both base and exponent 0.

 -}

-- 


