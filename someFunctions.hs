import Data.List

common::(Ord a)=>[a]->[a]->[a]
common xs [] = []
common [] ys = []
common xs@(x:xt) ys@(y:yt)
    |x<y  = common xt ys
    |x==y = x:common xt yt
    |x>y  = common xs yt

diff::(Ord a)=>[a]->[a]->[a]
diff xs [] = xs
diff [] ys = []
diff xs@(x:xt) ys@(y:yt)
    |x<y  = x:diff xt ys
    |x==y = diff xt yt
    |x>y  = diff xs yt

merge::(Ord a)=>[a]->[a]->[a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x:xt) ys@(y:yt)
    |x<y  = x:merge xt ys
    |x==y = x:merge xt yt
    |x>y  = y:merge xs yt

merge'::(Ord a)=>[a]->[a]->[a]
merge' [] ys = ys
merge' xs [] = xs
merge' xs@(x:xt) ys@(y:yt)
    |x<y  = x:merge' xt ys
    |x==y = x:y:merge' xt yt
    |x>y  = y:merge' xs yt

group'::(Ord a)=>[a]->[[a]]
group' (x:xt) = (x:prefix):group' remainder
    where (prefix,remainder) = span (==x) xt

legendreDePolignac::Int->Int->Int
legendreDePolignac n m = f 0 m
    where f k s
            |n<s       = k
            |otherwise = f (k+(n`quot`s)) (s*m)
    
multiples::Int->[Int]
multiples n = [n,2*n..]

commonMultiples::[Int]->[Int]
commonMultiples xs = foldl1 common.map multiples $xs

mcmList::[Int]->Int
mcmList xs = foldl1' lcm xs

gcdList::[Int]->Int
gcdList xs = foldl1' gcd xs

listPrimes::[Int]
listNonPrimes::[Int]
listPrimes    = 2:3:5:7:(diff [9,11..] listNonPrimes) 
listNonPrimes = foldr1 f.map g.tail $listPrimes
    where f (x:xt) ys = x:(merge xt ys)
          g p         = [p*p,p*(p+2)..]

listPrimesLow::Int->[Int]
listPrimesLow n = takeWhile (<=n) listPrimes

isPrime::Int->Bool
isPrime m = m`elem`listPrimes

firstPrimes::Int->[Int]
firstPrimes n = take n listPrimes

nPrime::Int->Int
nPrime n = listPrimes!!n

primeFactors::Int->[Int]
primeFactors n = listFactors n listPrimes
    where listFactors n (p:pt)
             |p*p>n      = [n]
             |n`rem`p/=0 = listFactors n pt
             |otherwise  = p:listFactors (n`quot`p) (p:pt)

listFactors'::Int->[[Int]]
listFactors' n = group $primeFactors n

primeFactors'::Int->[(Int,Int)]
primeFactors' n = map elemLength $listFactors' n
    where elemLength xs = (head xs,length xs)

numDiv::Int->Int
numDiv n = product.map (\xs->1+length xs).listFactors' $n

phi::Int->Int
phi n = n+1-(numDiv n)

productsList::[Int]->[Int]
productsList'::[Int]->[Int]->[Int]
productsList xs = productsList' [] xs
productsList' ys [] = ys
productsList' ys (x:xt) = productsList' (f ys x) xt
    where f [] x = [x]
          f (y:yt) x = y:(y*x):(f yt x)

nubProductsList::[Int]->[Int]
nubProductsList'::[Int]->[Int]->[Int]
nubProductsList xs = nubProductsList' [] xs
nubProductsList' ys [] = ys
nubProductsList' ys (x:xt) = nubProductsList' (nub.f ys $x) xt
    where f [] x = [x]
          f (y:yt) x = y:(y*x):(f yt x)

listDiv::Int->[Int]
listDiv 0 = []
listDiv n = (1:).nubProductsList.primeFactors $n

sumProductsList::[Int]->Int
sumProductsList xs = foldl1 (\acc x->acc*(x+1)) $xs

listFib::[Int]
listFib = 0:1:(zipWith (+) listFib $tail listFib)

nFib::Int->Int
nFib n = listFib!!n

listTriangNum::[Int]
listTriangNum = scanl1 (+) [1..]

listPentNum::[Int]
listPentNum = scanl1 (+) [1,4..]

listHexNum::[Int]
listHexNum = scanl1 (+) [1,5..]

listFact::[Integer]
listFact = scanl (*) 1 [1..]

nFact::Int->Integer
nFact n = listFact!!n

tartaLP::Int->Int->Integer
tartaLP l p = (nFact l)`quot`((nFact p)*(nFact $l-p))

collantzChain::Int->[Int]
collantzChain n
   |n==1      = [1]
   |even n    = n:collantzChain (n`quot`2)
   |otherwise = n:collantzChain (3*n+1)
