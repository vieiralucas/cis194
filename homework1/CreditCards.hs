toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (a : b : xs) = [a, b * 2] ++ doubleEveryOther xs
doubleEveryOther n = n

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (n : [])
  | n < 10 = n
  | otherwise = sumDigits (toDigits n)
sumDigits (n:xs)
  | n < 10 = n + (sumDigits xs)
  | otherwise = sumDigits ((toDigits n) ++ xs)

validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigitsRev n)) `mod` 10) == 0
