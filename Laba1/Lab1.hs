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
       
testPower1 n k = power1 n k == n^k

-- C -------------------------
-- power2

power2 n k  | k==0 = 1
            | k > 0 && isEven= power (n * n) (k `div` 2)
            | k > 0 = n * (power n (k-1))
         where isEven = (k `mod` 2) == 0

testPower2 n k = power2 n k == n^k

-- D -------------------------
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


