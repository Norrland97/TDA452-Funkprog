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
            | k < 0 = error "power: negative argument"
       
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

testAllPower :: Integer -> Integer -> Bool
testAllPower n k = testPower1 n k == testPower2 n k



printTest bos  | length bos == 0 = []
               | otherwise       = show (head bos) : printTest (tail bos)

runTest = runTest' nTestValues kTestValues  

runTest' :: [Integer] -> [Integer] -> [Bool]
runTest' ns ks | length ns == 0 = []
               | otherwise      = testAllPower (head ns) (head ks) : runTest' (tail ns) (tail ks)

nTestValues :: [Integer]
nTestValues = [3,6,0,1,5,-2,10000000,3,4,2]

kTestValues :: [Integer]
kTestValues = [6,2,7,10,0,4,4,3,20000,-5]

{- 

<Describe your test cases here>

 -}

-- 
prop_powers = undefined

--
powerTest :: Bool
powerTest = undefined

--
prop_powers' = undefined


